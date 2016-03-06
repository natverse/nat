#' A simple wrapper class for multiple transformations
#' 
#' @description A \code{reglist} is read as a set of transformations to be 
#'   applied sequentially starting with the first element, then applying the 
#'   second transformation to the result of the first and so on. Each individual
#'   transformation is considered to map data from the sample (floating/moving) 
#'   space to the reference (fixed/template) space.
#'   
#'   Each transformation may have an attribute \code{"swap"} indicating that the
#'   natural direction of the transformation should be swapped (i.e. inverted). 
#'   This can be done trivially in the case of affine transformations, 
#'   expensively for others such as CMTK registrations (see 
#'   \code{\link{cmtkreg}}) and not at all for others. Note that the term 'swap'
#'   is used to avoid a direct equivalence with inversion - many registration 
#'   tools use the term \emph{inverse} for directions that one might naively 
#'   think of as as the natural direction of the transformation (see 
#'   \code{\link{xformpoints.cmtkreg}} for discussion).
#'   
#' @param ... One or more transformations/reglists to combine
#' @param swap A vector of the same length as \code{...} indicating whether the 
#'   direction of each transformation should be swapped (i.e. mapping reference 
#'   -> sample).
#' @details The swap argument is provided as a convenience, but an attribute
#'   \code{'swap'} can also be set directly on each registration.
#' @export
#' @seealso \code{\link{xform}}
reglist <- function(..., swap=NULL) {
  l=list(...)
  if(length(l)==1 && inherits(l[[1]], 'reglist')) l=l[[1]]
  if(!is.null(swap)){
    if(length(swap)!=length(l)) 
      stop("swap must have the same length as the number of registrations!")
    l=mapply(function(x, s) {attr(x,'swap')=s;x}, l, swap, SIMPLIFY = FALSE)
  } 
  class(l)='reglist'
  l
}

#' @export
"[.reglist" <- function(x, i) {
  structure(NextMethod(), class='reglist')
}

#' @description \code{c.reglist} combines multiple \code{reglist}s into a single
#'   \code{reglist}.
#'   
#' @param recursive Presently ignored
#' @export
#' @seealso \code{\link[base]{c}}
#' @rdname reglist
#' @examples
#' I=diag(4)
#' S=I
#' diag(S)=c(1, 2, 3, 1)
#' rl=reglist(S, I)
#' rl2=c(rl, 'path/to/my/reg.list')
#' rl3=c(reglist('path/to/my/reg.list'), rl)
c.reglist<-function(..., recursive = FALSE){
  structure(NextMethod(), class='reglist')
}


# Helper function indicating if a registration/reglist has the swap attribute
swapped<-function(x){
  if(inherits(x, 'reglist')) return(sapply(x, swapped))
  isTRUE(attr(x, 'swap'))
}

# Helper function to remove tempfiles
unlinktempfiles_reglist<-function(reg){
  if(length(tfs<-attr(reg, 'tempfiles')))
    unlink(tfs, recursive = TRUE)
}

#' Simplify a registration list
#' 
#' @details This function \itemize{
#'   
#'   \item inverts any affine matrices with attribute \code{"swap"}
#'   
#'   \item collapses multiple affine matrices into a single affine
#'   
#'   \item optionally converts all registrations to CMTK on disk registrations 
#'   when possible.
#'   
#'   }
#'   
#'   Note that if any of the registrations are in CMTK format, the default 
#'   behaviour is to try to convert all of the other registrations into CMTK 
#'   format to enable them to be passed to CMTK in a single command. If 
#'   \code{as.cmtk=TRUE} then there will be an error if this is not possible.
#'   
#' @param reg A registration list (\code{\link{reglist}}) containing one or more
#'   transformations.
#' @param as.cmtk Whether to convert to a vector of CMTK format registrations 
#'   (see \code{\link{cmtkreg}}). The default value of \code{as.cmtk=NULL} 
#'   converts all registrations to CMTK if any one registration is in CMTK 
#'   format (thus enabling them to be applied by CMTK tools in a single call).
#'   See details.
#' @export
#' @seealso \code{\link{reglist}}, \code{\link{xform}}, \code{\link{cmtkreg}}
simplify_reglist<-function(reg, as.cmtk=NULL) {
  regclasses <- sapply(reg, function(x) class(x)[1], USE.NAMES = FALSE)
  
  # first invert any affine matrices
  sw=swapped(reg)
  mats_to_invert=sw & regclasses=="matrix"
  if(any(mats_to_invert)) {
    reg[mats_to_invert]=lapply(reg[mats_to_invert], solve)
  }
  # then compose affine transformations into single matrix
  if(isTRUE(sum(regclasses=="matrix")>1)){
    # note that this needs to be done in reverse order to match the order in
    # which matrix multiplication would otherwise happen
    for(i in rev(seq_along(reg)[-1])){
      if(isTRUE(all(regclasses[c(i,i-1)]=='matrix'))){
        reg[[i-1]]=reg[[i]]%*%reg[[i-1]]
        reg[[i]]=NULL
        regclasses=regclasses[-i]
      }
    }
  }
  if(is.null(as.cmtk) && any(regclasses%in% c("cmtkreg", "character")) && 
     all(regclasses%in% c("cmtkreg", "character", "matrix"))) {
    # we only have cmtk and homogeneous affine transforms so let's simplify
    # to turn this into a single CMTK call
    as.cmtk=TRUE
  }
  if(isTRUE(as.cmtk)) {
    # convert any affine matrices to cmtkreg
    tfs=NULL
    if(nm<-sum(regclasses=="matrix")) {
      tfs=as.cmtkreg(replicate(nm, tempfile(fileext = ".list")))
      mapply(write.cmtkreg, reg[regclasses=="matrix"], tfs)
      reg[regclasses=="matrix"]=tfs
    }
    reg=as.cmtkreg(reg)
    # so we can delete tempfiles later (no need to have cmtkreg class)
    if(length(tfs)) attr(reg,'tempfiles')=unclass(tfs)
  }
  reg
}
