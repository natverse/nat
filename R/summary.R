#' Summary statistics for neurons in a neuronlist (e.g. cable length, nodes)
#' 
#' @param object The \code{neuronlist} to summarise
#' @param ... Additional arguments to summary methods for neuronlist objects
#' 
#' @export
#' @examples 
#' summary(Cell07PNs)
summary.neuronlist<-function(object, ...) {
  l=lapply(object, summary)
  d=do.call(rbind, l)
  cbind(data.frame(object),d)
}

#' Summary statistics for individual neurons
#' @param object The \code{neuron} to summarise
#' @param ... Additional arguments (currently ignored)
#' @export
#' @examples 
#' summary(Cell07PNs[[1]])
summary.neuron<-function(object, ...) {
  data.frame(nodes=nrow(object$d), cable.length=sum(seglengths(object)))
}

#' @export
#' @param veclength The vector length to assume for each segment so that a cable
#'   length estimate can be made.
#' @rdname summary.neuron
#' @examples 
#' summary(kcs20[[1]])
summary.dotprops<-function(object, veclength=1, ...) {
  data.frame(nodes=nrow(object$points), 
             cable.length=nrow(object$points)*veclength)
}
