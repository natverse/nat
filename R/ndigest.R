#' Calculated normalised digest value for an object
#' 
#' @description The \emph{normalised} digest should exclude any fields or 
#'   attributes irrelevant to the core contents of the object (e.g. timestamps, 
#'   absolute location of the input files on disk etc). In theory then, this
#'   value should be constant for the same data regardless of the particular
#'   machine on which the digest is being computed.
#' @param x Object for which a normalised digest will be computed.
#' @param ... Additional arguments passed to methods and then on to 
#'   \code{\link{digest}}
#' @return A character string containing the digest of the supplied object 
#'   computed by \code{\link{digest}}.
#' @export
#' @importFrom digest digest
#' @seealso \code{\link{digest}}
ndigest<-function(x, ...) UseMethod('ndigest')

#' @details \code{ndigest.neuronlistfh} ignores all references to local and 
#'   remote paths embedded in the \code{\link{neuronlistfh}} object, the 
#'   filehash object and any hashmap.
#' @method ndigest neuronlistfh
#' @export
#' @rdname ndigest
ndigest.neuronlistfh<-function(x, ...){
  attr(x,'remote')=NULL
  attr(x,'file')=NULL
  attr(x,'db')=NULL
  digest(x, ...)
}

#' @details \code{ndigest.dotprops} ignores any \code{mtime} or \code{file}
#'   attributes. It also converts tangent vectors to absolute values (when 
#'   \code{absoluteVectors=TRUE}) because the direction vectors are computed 
#'   using an eigenvector decomposition where the sign of the eigenvector is 
#'   essentially random and subject to small numerical instabilities. Therefore 
#'   it does not usually make sense to rely on the value of vect exactly.
#' @inheritParams all.equal.dotprops
#' @rdname ndigest
#' @seealso \code{\link{all.equal.dotprops}}
ndigest.dotprops<-function(x, absoluteVectors=TRUE, ...){
  # remove mtime and file attributes
  atts=attributes(x)
  mostattributes(x)<-atts[setdiff(names(atts),c("mtime",'file'))]
  if(absoluteVectors)
    x$vect=abs(x$vect)
  digest(x, ...)
}

#' @export
ndigest.default<-function(x, ...){
  digest(x, ...)
}