#' Calculated normalised digest value for an object
#' 
#' @description The \emph{normalised} digest should exclude any fields or attributes 
#'   irrelevant to the core contents of the object (e.g. timestamps, absolute
#'   location of the input files )
#' @export
#' @importFrom digest digest
ndigest<-function(x, ...) UseMethod('ndigest')

#' @details ndigest.neuronlistfh ignores all references to local and remote
#'   paths embedded in the \code{neuronslistfh} object.
#' @method ndigest neuronlistfh
#' @export
#' @rdname ndigest
ndigest.neuronlistfh<-function(x, ...){
  attr(x,'remote')=NULL
  attr(x,'file')=NULL
  attr(x,'db')@dir=NULL
  digest(x, ...)
}

#' @export
ndigest.default<-function(x, ...){
  digest(x, ...)
}
