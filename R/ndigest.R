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

#' @details \code{ndigest.neuronlistfh} only considers the \code{keyfilemap} and
#'   \code{df} (metadata data.frame) when computing the hash value. See
#'   \code{\link{neuronlistfh}} for the significance of these two fields.
#' @method ndigest neuronlistfh
#' @export
#' @rdname ndigest
ndigest.neuronlistfh<-function(x, ...){
  # we
  digest(attributes(x)[c("keyfilemap","df")], ...)
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
#' @export
#' @examples
#' stopifnot(all.equal(ndigest(kcs20[[1]]), "4c045b0343938259cd9986494fc1c2b0"))
ndigest.dotprops<-function(x, absoluteVectors=TRUE, ...){
  # remove mtime and file attributes
  atts=attributes(x)
  mostattributes(x)<-atts[setdiff(names(atts),c("mtime",'file'))]
  if(absoluteVectors)
    x$vect=abs(x$vect)
  digest(x, ...)
}

#' @details \code{ndigest.neuron} ignores the following fields:
#'   
#'   \itemize{
#'   
#'   \item InputFileName
#'   
#'   \item CreatedAt
#'   
#'   \item NodeName
#'   
#'   \item InputFileStat
#'   
#'   \item InputFileMD5
#'   
#'   }
#' @param fieldsToExclude Character vector naming the neuron fields to exclude
#' @rdname ndigest
#' @export
#' @seealso \code{\link{all.equal.neuron}}
ndigest.neuron<-function(x, fieldsToExclude=c("InputFileName","CreatedAt",
                                              "NodeName","InputFileStat",
                                              "InputFileMD5"), ...){
  fieldsToKeep=setdiff(names(x), fieldsToExclude)
  digest(x[fieldsToKeep], ...)
}

#' @export
ndigest.default<-function(x, ...){
  digest(x, ...)
}
