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
#' @param ... One or more transformations
#' @param swap A vector of the same length as \code{...} indicating whether the 
#'   direction of each transformation should be swapped (i.e. mapping reference 
#'   -> sample).
#' @details The swap argument is provided as a convenience, but an attribute
#'   \code{'swap'} can also be set directly on each reigstration.
#' @export
#' @seealso \code{\link{xform}}
reglist <- function(..., swap=NULL){
  l=list(...)
  if(!is.null(swap)){
    l=mapply(function(x, s) {attr(x,'swap')=s;x}, l, swap)
  } 
  class(l)='reglist'
  l
}
