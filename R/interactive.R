### Functions for interactively working with neurons ###

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
#' pruned.as.you.like.it = prune_online(Cell07PNs[1:2])
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
  ids=integer()
  while(!continue%in%c("y","yes")){
    selected = select_points(xyzmatrix(x), clear_plot_on_exit = TRUE)
    v = match(data.frame(t(selected)), data.frame(t(xyzmatrix(x))))
    neuron = prune_vertices(x, verticestoprune=v,invert=TRUE)
    pop3d(id=unlist(ids))
    ids=rgl::plot3d(neuron, col ="black")
    continue = readline("Finished with this neuron? yes/no ")
    pop3d(id=unlist(ids))
  }
  neuron
}
#' @export
#' @rdname prune_online
prune_online.neuronlist <- function(x, ...){
  nlapply(x,prune_online.neuron)
}


#' Interactively re-root neurons (usually to their soma)
#'
#' @description Cycle through and manually re-root neurons using an rgl window.
#'   This will typically be used for manual identification of the soma of a
#'   neuron.
#'
#' @param someneuronlist a neuron/neuronlist object
#' @param brain for context, plot some other objects (e.g. a brain from the
#'   nat.templatebrains package such as FCWB, or any object that may be plotted
#'   using plot3d)
#' @examples
#' \dontrun{
#' ## NB these neurons actually have their somata chopped off
#' correctedsomas = correct_root(Cell07PNs[1:3])
#' plot3d(correctedsomas, soma=TRUE)
#' }
#' @return a matrix of 3D points
#' @export
correct_root <- function(someneuronlist, brain = NULL){
  correctedsomas = someneuronlist
  nopen3d()
  for (n in 1:length(someneuronlist)){
    message("neuron ", n, " of ", length(someneuronlist))
    w = someneuronlist[[n]]
    print(names(someneuronlist[n]))
    if(!is.null(brain)){rgl::plot3d(brain)}
    rgl::plot3d(w, soma = T)
    eps.df=w$d[endpoints(w),]
    progress =F
    while(!isTRUE(progress)){
      cat ("Rotate brain and then hit [enter] to continue")
      line <- readline()
      message("Select new root from highlighted endpoints")
      selection <- select3d()
      selected.point <- selection(xyzmatrix(eps.df))
      selected.pointno <- eps.df$PointNo[selected.point]
      if (length(selected.pointno)!=1){
        message("You must select exactly one end point. Try again!")
      } else{
        corrected = reroot(w, id=selected.pointno)
        rgl::plot3d(corrected, soma = T, col = "blue")
        progress = tolower(readline(prompt="Good enough? [y/n] "))=="y"
      }
    }
    rgl::clear3d()
    correctedsomas[[n]]=corrected
  }
  correctedsomas
}


#' Generate a 3D model from connector and/or tree node data
#'
#' @description Generate a mesh3d model based on points contained in a
#'   neuronlist or neuron object, or another object that consists of 3D points.
#'
#' @param x a neuronlist or neuron object, or another object that consists of 3D points
#' @param substrate whether to make the model based on the 3D location of
#'   connectors, neuron cable or both. Connectors are pre-synapse locations, 
#'   e.g. the pre-synapses of a \code{catmaidneuron} from the R package \code{catmaid})
#' @param alpha a single value or vector of values for alpha, fed to
#'   \code{alphashape3d::ashape3d}. Selection is subsequently interactive.
#' @param auto.selection logical, whether or not to try and remove points based on interactively
#'   choosing simple values for clustering.
#' @details Interactive function that allows a users to select points in 3D space from neuronlist/neuron objects,
#' or another object that is coercible in 3D points using \code{\link{xyzmatrix}}. Points can first be automatically chosen, by
#' selecting an integer number of nearest neighbours to find for each point using \code{nabor::\link{knn}}, and then a 
#' maximum distance at which nodes can be part of a cluster. Next, \code{\link{select_points}} is used to manually pick desired 3D
#' points. Lastly, \code{alphashape3d::ashape3d} is used to create an alphashape around these points. The user can trial different values for
#' alpha until they get their desired result.
#' @examples
#' \dontrun{
#' # Make a model based off of fly olfactory projection neuron arbours
#' PN_blob = make_model(Cell07PNs)
#' }
#' @seealso \code{\link{prune_online}}
#' @return A mesh3d object
#' @export
make_model <- function(x, 
                       substrate = c("cable", "connectors", "both"), 
                       alpha = 30, 
                       auto.selection = TRUE){
  # need alphashape3d
  if(!requireNamespace("alphashape3d", quietly = TRUE))
    stop("Please install the suggested alphashape3d package in order to use make_model!")
  
  # get starting points
  substrate <- match.arg(substrate)
  if (is.neuron(x)|is.neuronlist(x)) {
    if (substrate=="connectors"){substrate.points <- xyzmatrix(do.call(rbind, lapply(x, function(x) x$connectors)))
    } else if(substrate =="cable"){substrate.points <- xyzmatrix(x) 
    } else if (substrate == "both"){substrate.points <- rbind(xyzmatrix(x), do.call(rbind, lapply(x, function(x) xyzmatrix(x$connectors))))}
  }else{
    substrate.points  <-  xyzmatrix(x)
  }
  
  # automatic point selection
  if (auto.selection == TRUE){
    progress = "n"
    while (progress == "n"){
      groupsize <- as.numeric (readline(prompt="Each node must have this number of near neighbours (e.g. 10): "))
      maxdistance <- as.numeric (readline(prompt="And all the neighbours must be this close in Euclidean space (e.g. 1):  "))
      if(groupsize<1){
        groupsize <- 1
      }
      if(maxdistance<0){
        maxdistance <- 0
      }
      neighbours <- nabor::knn(substrate.points, substrate.points, k = groupsize)
      loose <- apply(neighbours$nn.dists, 1, function(x) {(any(as.numeric(x[1:ncol(neighbours$nn.dists)]) > maxdistance))})
      keep <- c(neighbours$nn.idx[,1][!loose])
      selected.points <- substrate.points[keep,]
      message("Selected points in black, deselected in red")
      ids <- rgl::points3d(selected.points, cl = 'black')
      ids  <-  union(ids, rgl::points3d(substrate.points, col = 'red') )
      progress  <-  readline(prompt="Continue to manual selection/deselection? y/n  ")
      pop3d(id=ids)
    }
  }
  
  # manual point selection
  progress = "p"
  while (progress != "c"){
    if (progress == 'r'){
      removed.points <- picked.points(selected.points)
      selected.points <- subset(selected.points, !removed.points)
    }
    if (progress == 'a'){
      added.points <- subset(substrate.points, picked.points(substrate.points))
      selected.points <- rbind(selected.points, added.points)
    }
    ids <- rgl::points3d(selected.points, cl = 'black')
    ids  <-  union(ids, rgl::points3d(substrate.points, col = 'red') )
    progress  <-  readline(prompt="Selected points in black. Remove (r) or add (a) points? Or continue to alphashape generation (c)?  ")
    if(progress != "c"){
      picked.points <- rgl::select3d() 
      pop3d(id=ids)
    }
  } 
  
  # make alphashape
  ids <- rgl::points3d(selected.points, cl = 'black')
  ids  <-  union(ids, rgl::points3d(substrate.points, col = 'red') )
  progress = "n"
  while (progress == "n"){
    message("alpha is ", alpha)
    alphashape <- alphashape3d::ashape3d(unique(selected.points), alpha = alpha)
    mesh3d <- as.mesh3d(alphashape)
    ids=rgl::plot3d(mesh3d, alpha = 0.5, col = "purple", add = TRUE)
    progress <- readline(prompt="Continue? y/n  ")
    if(progress == "n"){
      alpha <- as.numeric (readline(prompt="Select a numeric value for alpha  "))
    }
  }
  
  # return
  mesh3d
}

#' Interactively select 3D points in space
#'
#' @description Plot a set of 3D points in space and select a subset of them
#'   interactively, using an rgl window
#'
#' @param points a matrix of 3D points to plot (or an object for which
#'   \code{\link{xyzmatrix}} can extract 3D points).
#' @param clear_plot_on_exit Whether to remove points from the rgl scene when
#'   selection has been completed.
#' @examples
#' \dontrun{
#' # Select points from 3 olfactory projection neurons
#' selected_points = select_points(Cell07PNs[1:3])
#' }
#' @seealso \code{\link{prune_online}}
#' @return A matrix describing selected 3D points
#' @export
select_points <- function (points, clear_plot_on_exit=FALSE) {
  selected.points <- points <- xyzmatrix(points)
  ids=rgl::points3d(selected.points)
  progress = readline(prompt = "Selected points in black. Add (a) or remove (r) points, or continue (c)?  ")
  while (progress != "c") {
    if (progress == "a") {
      keeps = rgl::select3d()
      keep.points <- keeps(points)
      keep.points = subset(points, keep.points)
      selected.points = rbind(selected.points, keep.points)
    }
    if (progress == "r") {
      remove.points <- select3d()
      removed.points <- remove.points(selected.points)
      selected.points = subset(selected.points, !removed.points)
    }
    pop3d(id=ids)
    
    ids=integer()
    if (length(selected.points) > 0) {
      ids=rgl::points3d(selected.points)
    }
    ids=union(ids, rgl::points3d(points, col = "red"))
    progress = readline(prompt = "Add (a) or remove (r) points, or continue (c)?  ")
  }
  if(clear_plot_on_exit)
    pop3d(id=ids)
  return(selected.points)
}

