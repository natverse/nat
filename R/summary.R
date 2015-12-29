#' Summary statistics for neurons (e.g. cable length, number of nodes)
#' 
#' @description \code{summary.neuronlist} computes tree statistics for all the 
#'   neurons in a neuronlist object
#' @param object The neuron or neuronlist to summarise
#' @param ... For \code{summary.neuronlist} additional arguments passed on to 
#'   summary methods for individual neurons
#' @param include.attached.dataframe Whether to include the neuronlists attached
#'   metadata in the returned data.frame.
#'   
#' @export
#' @rdname summary.neuron
#' @seealso \code{\link{seglengths}}
#' @examples 
#' # summary for a whole neuronlist
#' summary(Cell07PNs)
#' # including the attached data.frame with additional metadata
#' head(summary(Cell07PNs, include.attached.dataframe = FALSE))
summary.neuronlist<-function(object, ..., include.attached.dataframe=FALSE) {
  l=lapply(object, summary)
  d=do.call(rbind, l)
  if(include.attached.dataframe) cbind(data.frame(object),d) else d
}

#' @description \code{summary.neuron} computes statistics for individual neurons
#' @return A \code{data.frame} summarising the tree properties of the neuron 
#'   with columns \itemize{
#'   
#'   \item{root}
#'   
#'   \item{nodes}
#'   
#'   \item{segments}
#'   
#'   \item{branchpoints}
#'   
#'   \item{endpoints}
#'   
#'   \item{cable.length}
#'   
#'   }
#' @export
#' @examples 
#' # for a single regular format neuron
#' summary(Cell07PNs[[1]])
summary.neuron<-function(object, ...) {
  data.frame(root=object$StartPoint,
             nodes=nrow(object$d),
             segments=object$NumSegs,
             branchpoints=length(object$BranchPoints),
             endpoints=length(object$EndPoints),
             cable.length=sum(seglengths(object)))
}

#' @description \code{summary.dotprops} computes statistics for individual
#'   neurons in dotprops format. Note the \code{veclength} argument.
#' @export
#' @param veclength The vector length to assume for each segment so that a cable
#'   length estimate can be made.
#' @rdname summary.neuron
#' @examples
#' # for a single dotprops format neuron
#' summary(kcs20[[1]])
#' # specify a different estimate for the cable length associated with a single
#' # point in the neuron
#' summary(kcs20[[1]], veclength=1.2)
summary.dotprops<-function(object, veclength=1, ...) {
  data.frame(nodes=nrow(object$points), 
             cable.length=nrow(object$points)*veclength)
}
