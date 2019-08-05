## Functions for interactively working with neurons

#' Prune a neuron interactively in an rgl window
#'
#' @description Remove points from a neuron, keeping the root node intact.
#' @details The neuron is plotted initially with all nodes selected (and shown
#'   with black points). You can interactively select points to remove (they
#'   will now be plotted in red). You can also add points back again (they will
#'   return to black). When you are finished, press [e] to exit and then indicate
#'   that you have finished (yes).
#' @param ... Additional methods passed to \code{prune_vertices}
#' @inheritParams prune
#' @return A pruned neuron/neuronlist object
#' @examples
#' \dontrun{
#' ## Interactively choose which bit of the neuron you wish to keep
#' pruned.as.you.like.it = prune_online(Cell07PNs[1])
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
    rgl::clear3d();rgl::plot3d(neuron, col ="black")
    continue = readline("Finished with this neuron? yes/no ")
  }
  neuron
}
#' @export
#' @rdname prune_online
prune_online.neuronlist <- function(x, ...){
  nlapply(x,prune_online.neuron)
}

#' Manually assign a dendrite and axon to a neuron
#'
#' @description Manually assign the dendrite and axon to neurons / a neuron
#'
#' @param x a neuron/\code{\link{neuronlist}} object
#' @inheritParams plot3d.neuron
#' @return The neuron/neuronlist object with axon/dendrite info assigned in SWC
#'   format to neuron$d
#' @seealso \code{\link{subset.neuron}}, \code{\link{prune.neuron}}
#' @examples
#' \dontrun{
#' ## Interactively choose which bit of the neuron you wish to keep
#' split.as.you.like.it = manually_assign_axon_dendrite(Cell07PNs[1:2])
#' }
#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite <-function(x, soma = FALSE) UseMethod("manually_assign_axon_dendrite")

#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite.neuron <- function(x, soma = FALSE){
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
    x$d$Label[rootpoints(x)] = 1
    rgl::clear3d()
    rgl::plot3d(dend,col="blue")
    rgl::plot3d(axon, col = "orange")
    rgl::plot3d(x, col = "purple", soma = soma)
    happy = readline("Happy with this division? yes/no  ")
  }
  x
}

#' @export
#' @rdname manually_assign_axon_dendrite
manually_assign_axon_dendrite.neuronlist<-function(x, soma = FALSE){
  nlapply(x, manually_assign_axon_dendrite.neuron, soma = soma)
}

#' Interactively re-root neurons to their soma
#'
#' @description Cycle through and manually re-root neurons to their soma 
#' using an rgl window
#'
#' @param someneuronlist a neuron/neuronlist object
#' @param brain for context, plot some other objects (e.g. a brain from the nat.templatebrains package such as FCWB,
#' or any object that may be plotted using plot3d)
#' @examples
#' \dontrun{ 
#' ## Admittedly, these neurns have their somata chopped off! 
#' correctedsoma = correct_soma(Cell07PNs)
#' } 
#' @return a matrix of 3D points
#' @export
correct_soma <- function(someneuronlist, brain = NULL){
  correctedsomas = neuronlist()
  nopen3d()
  for (n in 1:length(someneuronlist)){
    message("neuron ", n, " of ", length(someneuronlist))
    w = someneuronlist[[n]]
    print(names(someneuronlist[n]))
    if(!is.null(brain)){rgl::plot3d(brain)}
    rgl::plot3d(w, soma = T)
    eps.xyz=w$d[endpoints(w),]
    progress =F
    while(progress == F){
      cat ("Rotate brain and then hit [enter] to continue")
      line <- readline()
      message("Select new root from highlighted endpoints")
      selected.point <- select3d()
      selected.point <- selected.point(xyzmatrix(eps.xyz))
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


#' Generate a 3D model from connector and/or tree node data
#'
#' @description Generate a mesh3d model based on points contained in a
#'   neuronlist or neuron object
#'
#' @param someneuronlist a neuronlist or neuron object
#' @param substrate whether to make the model based off of connectors, neuron
#'   cable or both
#' @param auto.selection whether to try and remove points based on interactively
#'   chosen values for \code{groupsize} and \code{maxdistance}
#' @param maxdistance for automated cluster identification. Maximum distance at
#'   which nodes can be part of a cluster
#' @param groupsize an integer number of nearest neighbours to find using
#'   nabor::knn()
#' @param selection whether or not to interactively select values for
#'   maxdistance and groupsize.
#' @param alpha a single value or vector of values for alpha, fed to \code{shape3d}.
#'   Selection is subsequently interactive
#' @param chosen.points a matrix of 3D points. Use this argument if you do not
#'   want to interactively select the 3D fed to \code{ashape3d}.
#' @examples
#' \dontrun{
#' # Make a model based off of fly olfactory projection neuron arbours
#' PN_blob = make_model(Cell07PNs)
#' }
#' @seealso \code{\link{prune_online}}
#' @return A mesh3d object
#' @export
make_model <- function(someneuronlist, substrate = c("connectors","cable", "both"), 
                       maxdistance = 10, groupsize = 10, alpha = 30, 
                       auto.selection = TRUE, chosen.points = NULL){
  if (substrate=="connectors"){synapse.points<-xyzmatrix(do.call(rbind, lapply(someneuronlist, function(x) x$connectors)))
  }else if(substrate =="cable"){synapse.points<-xyzmatrix(someneuronlist) 
  }else if (substrate == "both"){synapse.points<-rbind(xyzmatrix(someneuronlist), do.call(rbind, lapply(someneuronlist, function(x) xyzmatrix(x$connectors))))}
  rgl::open3d()
  if (auto.selection == TRUE){
    progress = "n"
    while (progress == "n"){
      groupsize <- as.numeric (readline(prompt="Select a value for the cluster groupsize  "))
      maxdistance <- as.numeric (readline(prompt="Select a value for maximum distance between points  "))
      neighbours<-nabor::knn(synapse.points, synapse.points, k = groupsize)
      loose <- apply(neighbours$nn.dists, 1, function(x) {(any(as.numeric(x[1:ncol(neighbours$nn.dists)]) > maxdistance))})
      keep<-c(neighbours$nn.idx[,1][!loose])
      close.points<-synapse.points[keep,]
      rgl::clear3d();rgl::points3d(close.points, cl = 'black'); rgl::points3d(synapse.points, col = 'red')
      progress = readline(prompt="Continue? y/n  ")
    }
  }
  else{
    neighbours<-nabor::knn(synapse.points, synapse.points, k = groupsize)
    loose <- apply(neighbours$nn.dists, 1, function(x) {(any(as.numeric(x[1:ncol(neighbours$nn.dists)]) > maxdistance))})
    keep<- c(neighbours$nn.idx[,1][!loose])
    close.points<-synapse.points[keep,]
    rgl::clear3d();rgl::points3d(close.points, cl = 'black'); rgl::points3d(synapse.points, col = 'red')
  }
  # Manual point deselection
  selected.points = unique(close.points)
  if (!is.null(chosen.points)){ selected.points = chosen.points}
  progress = readline(prompt="Remove (r) or add (a) points? Or continue to alphashape generation (c)?  ")
  while (progress != "c"){
    if (progress == 'r'){
      remove.points <- rgl::select3d()
      removed.points <- remove.points(selected.points)
      selected.points<-subset(selected.points, !removed.points)
      rgl::clear3d(); rgl::points3d(selected.points); rgl::points3d(synapse.points, col = 'red')
    }
    if (progress == 'a'){
      add.points <- rgl::select3d()
      added.points<-subset(synapse.points, add.points(synapse.points))
      selected.points<-rbind(selected.points, added.points)
      rgl::clear3d(); rgl::points3d(selected.points); rgl::points3d(synapse.points, col = 'red')
    }
    progress = readline(prompt="Remove (r), add (a) or save (s) points?  ")
  }
  progress = "n"
  while (progress == "n"){
    alpha <- as.numeric (readline(prompt="Select a nmeric value for alpha  "))
    alphashape<-alphashape3d::ashape3d(unique(selected.points), alpha = alpha)
    plot(alphashape)
    progress<-readline(prompt="Continue? y/n  ")
  }
  as.mesh3d(alphashape)
}

#' Interactively select 3D points in space
#'
#' @description Plot a set of 3D points in space and select a subset of them
#'   interactively, using an rgl window
#'
#' @param points a matrix of 3D points to plot
#' @param plot3d additional object that can be plotted using \code{rgl::plot3d},
#'   to plot alongside points (e.g. for context)
#' @examples
#' \dontrun{
#' # Make a model based off of fly olfactory projection neuron arbours
#' selected_points = select_points(xyzmatrix(Cell07PNs))
#' }
#' @seealso \code{\link{prune_online}}
#' @return A matrix describing selected 3D points
#' @export
select_points <- function (points, plot3d = NULL) {
  rgl::plot3d(plot3d, col = "grey")
  points = xyzmatrix(points)
  selected.points = unique(points)
  rgl::points3d(selected.points)
  progress = readline(prompt = "Add (a) or remove (r) points, or exit (e)?  ")
  while (progress != "e") {
    if (progress == "a") {
      keeps = rgl::select3d()
      keep.points <- keeps(unique(points))
      keep.points = subset(unique(points), keep.points)
      selected.points = rbind(selected.points, keep.points)
      rgl::clear3d()
      rgl::plot3d(plot3d)
      rgl::points3d(selected.points)
      rgl::points3d(unique(points), col = "red")
    }
    if (progress == "r") {
      remove.points <- select3d()
      removed.points <- remove.points(selected.points)
      selected.points = subset(selected.points, !removed.points)
    }
    rgl::clear3d()
    rgl::plot3d(plot3d, col = "grey")
    if (length(selected.points) > 0) {
      rgl::points3d(selected.points)
    }
    rgl::points3d(unique(points), col = "red")
    progress = readline(prompt = "Add (a) or remove (r) points, or exit (e)?  ")
  }
  return(selected.points)
}

