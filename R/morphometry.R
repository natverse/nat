# Functions for morphological analysis


#' Generate a connectivity matrix based on euclidean distance between points
#'
#' @description Generates an 'overlap matrix' of overlap scores between neurons in the \code{outputneurons} and \code{inputneurons} pools.
#' For every point in a given neuron in \code{outputneurons}, a distance score is calculated to every point in a neuron in \code{inputneurons}.
#' The sum of this score is added to the final output matrix. The score is calculated as \code{e(-d^2/(2*delta^2))}, where d is the euclidean distance between the two points,
#' and delta is the expected distance in um that is considered 'close'. It is recommended that the user resamples neurons before use, using \code{\link{resample}}.
#'
#' @param outputneurons first set of neurons
#' @param inputneurons second set of neurons
#' @param delta the distance (in um) at which a synapse might occur
#' @param progress whether or not to have a progress bar
#' 
#' @examples
#' \dontrun{ 
#' # Calculate how much some neurons overlap with one another
#' ## Example requires the package nat.flybrains
#' Cell07PNs_overlap = overlap_score(outputneurons = Cell07PNs, inputneurons = Cell07PNs)
#' 
#' ## Plot the results
#' heatmap(Cell07PNs_overlap)
#' } 
#' @return a matrix of overlap scores
#' @seealso \code{\link{potential_synapses}}, \code{\link{resample}}
#' @export
overlap_score <- function(outputneurons, inputneurons, 
                    delta =1, progress = TRUE){
  score.matrix = matrix(0,nrow = length(outputneurons),ncol = length(inputneurons))
  rownames(score.matrix) = names(outputneurons)
  colnames(score.matrix) = names(inputneurons)
  for (n in 1:length(outputneurons)){
    a = xyzmatrix(outputneurons[[n]])
    inputneurons.d = nlapply(inputneurons, xyzmatrix, .progress = "none")
    s = sapply(inputneurons.d, function(x)sum(exp(-nabor::knn(query = a, data = x,k=nrow(x))$nn.dists^2/(2*delta^2)))) # Score similar to that in Schlegel et al. 2015
    score.matrix[n,] = s
    if(progress){
      nat_progress(x = n/length(outputneurons)*100, message = "calculating overlap")
    } 
  }
  score.matrix
}


#' Perform a Sholl analysis on neuron skeletons
#'
#' @description Functions for Sholl analysis of neuronal skeletons
#'
#' @param x a neuron or neuronlist object
#' @param start the origin from which spheres are grown for the Sholl analysis
#' @param starting.radius the radius of the first sphere. Defaults to the radius step
#' @param ending.radius the radius of the last sphere. If NULL the distance to the furthest dendritic point from the start point is taken
#' @param radius.step the change in radius between successive spheres. Defaults to one 100th of the radius of the ending sphere
#' @return a data.frame of spheres radii and the number of dendritic intersections at each radius
#' @examples
#' \dontrun{ 
#' # Calculate how much some neurons overlap with one another
#' ## Example requires the package nat.flybrains
#' Cell07PNs_sholl = sholl_analysis(x = Cell07PNs, radius.step = 1, ending.radius = 100)
#' head(Cell07PNs_sholl[[1]])
#' } 
#' @export
#' @rdname sholl_analysis
sholl_analysis <- function(x, start = colMeans(xyzmatrix(x)), 
                           starting.radius = radius.step, ending.radius = 1000, 
                           radius.step = ending.radius/100) UseMethod("sholl_analysis")
#' @export
#' @rdname sholl_analysis
sholl_analysis.neuron <- function(x, start = colMeans(xyzmatrix(x)), 
                           starting.radius = radius.step, ending.radius = 1000, 
                           radius.step = ending.radius/100){
  unit.vector <- function(x) {x / sqrt(sum(x^2))}
  dend = x$d
  dend$dists = nabor::knn(data = matrix(start,ncol=3), query = nat::xyzmatrix(x),k=1)$nn.dists
  if(is.null(ending.radius)){
    ending.radius = max(dend$dists)
  }
  radii = seq(from = starting.radius, to = ending.radius, by = radius.step)
  sholl = data.frame(radii = radii, intersections = 0)
  for(n in 1:length(radii)){
    r = radii[n]
    segments = x$SegList
    for(segment in segments){
      p = dend[segment,]
      dists = (nabor::knn(data = matrix(start,ncol=3), query = nat::xyzmatrix(p),k=1)$nn.dists - r) >= 0
      sholl[n,]$intersections = sholl[n,]$intersections + lengths(regmatches(paste(dists,collapse=""), gregexpr("TRUEFALSE|FALSETRUE", paste(dists,collapse=""))))
    }
  }
  sholl
}
#' @export
#' @rdname sholl_analysis
sholl_analysis.neuronlist <- function(x, start = colMeans(xyzmatrix(x)), 
                                  starting.radius = radius.step, ending.radius = 1000, 
                                  radius.step = ending.radius/100){
  nlapply(x, sholl_analysis.neuron, 
          start = start, starting.radius = starting.radius, ending.radius = ending.radius, radius.step = radius.step)
}
