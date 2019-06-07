## Functions for interactively working with neurons

#' Prune a neuron interactively in an rgl window
#'
#' @description Remove points from a neuron, keeping the root node intact
#'
#' @inheritParams prune
#' @return A pruned neuron/neuronlist object
#' @examples
#' \dontrun{ 
#' ## Interactively shoose which bit of the neuron you wish to keep
#' pruned.as.you.like.it = prune_online(Cell07PNs)
#' }
#' @seealso \code{\link{as.neuron.ngraph}}, \code{\link{subset.neuron}}, 
#'   \code{\link{prune.neuron}}
#' @export
#' @rdname prune_online
prune_online <-function(x, ...) UseMethod("prune_online")
#' @export
#' @rdname prune_online
prune_online.neuron <- function(x, ...){
  continue = "no"
  while(!continue%in%c("y","yes")){
    selected = select_points(xyzmatrix(x), plot3d = x)
    v = match(data.frame(t(selected)), data.frame(t(xyzmatrix(x))))
    neuron = prune_vertices(x,verticestoprune=v,invert=TRUE)
    rgl::clear3d();rgl::plot3d(neuron, col ="black",...)
    continue = readline("Finished with this neuron? yes/no ")
  }
  neuron
}
#' @export
#' @rdname prune_online
prune_online.neuronlist <- function(x, ...){
  nat::nlapply(x,prune_online.neuron)
}

#' Manually assign a dendrite and axon to a neuron
#'
#' @description Manually assign the dendrite and axon to neurons / a neuron
#'
#' @param x a neuron/neuronlist object
#' @param soma whether or not to plot a soma, and at what radius
#' @param ... additional arguments passed to methods
#' @return The neuron/neuronlist object with axon/dendrite info assigned in SWC format to neuron$d
#' #' @seealso \code{\link{as.neuron.ngraph}}, \code{\link{subset.neuron}}, 
#'   \code{\link{prune.neuron}}, \code{\link{prune_online}}
#' @examples
#' \dontrun{ 
#' ## Interactively shoose which bit of the neuron you wish to keep
#' split.as.you.like.it = manually_assign_axon_dendrite(Cell07PNs)
#' } 
#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite <-function(x, ...) UseMethod("manually_assign_axon_dendrite")
#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite.neuron <- function(x, ...){
  happy = "no"
  x$d$Label = 0
  while(!happy%in%c("y","yes")){
    rgl::clear3d()
    message("Please choose dendrites for your neuron ")
    dend = prune_online.neuron(x)
    x$d$Label[x$d$X%in%dend$d$X&x$d$Y%in%dend$d$Y] = 3
    rgl::clear3d()
    message("Please choose axon for your neuron ")
    axon = prune_online.neuron(x)
    x$d$Label[x$d$PointNo%in%axon$d$PointNo] = 2
    x$d$Label[nat::rootpoints(x)] = 1
    rgl::clear3d()
    rgl::plot3d(dend,col="blue")
    rgl::plot3d(axon, col = "orange")
    rgl::plot3d(x, col = "purple")
    happy = readline("Happy with this division? yes/no  ")
  }
  x
}
#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite.neuronlist<-function(x, ...){
  nat::nlapply(x, manually_assign_axon_dendrite.neuron, ...)
}

#' Interactively re-root neurons to their soma
#'
#' @description Cycle through and manually re-root neurons to their soma 
#' using an rgl window
#'
#' @param someneuronlist a neuron/neuronlist object
#' @param brain for context, plot some other objects (e.g. a brain from the nat.templatebrains package such as FCWB,
#' or any object that may be plotted using plot3d)
#' @param ... additional arguments passed to methods
#' @examples
#' \dontrun{ 
#' ## Admittedly, these neurns have their somata chopped off! 
#' correctedsoma = correct_soma(Cell07PNs)
#' } 
#' @return a matrix of 3D points
#' @export
correct_soma <- function(someneuronlist, brain = NULL,...){
  correctedsomas = neuronlist()
  nopen3d()
  for (n in 1:length(someneuronlist)){
    message("neuron ", n, " of ", length(someneuronlist))
    w = someneuronlist[[n]]
    print(names(someneuronlist[n]))
    if(!is.null(brain)){rgl::plot3d(brain)}
    rgl::plot3d(w, soma = T)
    eps.xyz=w$d[nat::endpoints(w),]
    progress =F
    while(progress == F){
      cat ("Rotate brain and then hit [enter] to continue")
      line <- readline()
      message("Select new root from highlighted endpoints")
      selected.point <- select3d()
      selected.point <- selected.point(nat::xyzmatrix(eps.xyz))
      selected.point <- eps.xyz$PointNo[selected.point]
      if (length(selected.point)>1|length(selected.point)==0){
        message("Multiple end points selected, try again")
      }else{
        corrected = as.neuron(as.ngraph(w), origin = selected.point)
        rgl::plot3d(corrected, soma = T, col = "blue")
        progress = readline(prompt="Good enough? T/F  ")
      }
    }
    rgl::clear3d()
    correctedsomas = c(correctedsomas, as.neuronlist(corrected))
  }
  correctedsomas[,] = someneuronlist[,]
  correctedsomas
}
