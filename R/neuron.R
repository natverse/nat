#' neuron: class to represent traced neurons
#'  
#' neuron objects consist of a list containing multiple fields describing the
#' 3D location and connectivity of points in a traced neuron. The critical fields
#' of a neuron, n, are n$d which contains a dataframe in SWC format and 
#' n$SegList which contains a representation of the neuron's topology used for
#' most internal calculations.  
#' Useful functions include
#' plot.neuron
#' plot3d.neuron
#' write.neuron
#' read.neuron 
#' @name neuron
#' @family neuron
#' @seealso neuronlist
NULL