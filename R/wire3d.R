#' Wire frame plots 
#' 
#' This function directs the wireframe plot based on the plotengine backend selected.
#' @param x object of type 'mesh3d' (which should be a triangular mesh), 'hxsurf' or 'shapelist3d'
#' @param plotengine Whether to use plotting backend of 'rgl' or 'plotly'
#' @param ... Additional arguments passed to \code{\link[rgl]{wire3d}} or 
#' \code{\link[plotly]{add_trace} depending on the @param plotengine option choosen}
#' @export
#' @examples 
#' \donttest{
#' library(alphashape3d)
#' kcs20.a=ashape3d(xyzmatrix(kcs20), alpha = 10)
#' plot(kcs20.a)
#' 
#' # convert to mesh3d
#' kcs20.mesh=as.mesh3d(kcs20.a)
#'
#' nclear3d()
#' options(nat.plotengine = 'plotly')
#' wire3d(kcs20.mesh,alpha = 0.1, col = 'blue')
#' }
wire3d <- function(x, ..., plotengine = getOption('nat.plotengine')) {
  plotengine <- check_plotengine(plotengine)
  if(plotengine == 'plotly') {
    class(x)=c(paste0("plotly", class(x)[1]), class(x))
  }
  UseMethod('wire3d',x)
}

#' @method wire3d hxsurf
#' @export
wire3d.hxsurf <- function(x, Regions=NULL, ...) {
  wire3d(as.mesh3d(x, Regions=Regions), ...)
}

#' @method wire3d plotlyhxsurf
#' @export
wire3d.plotlyhxsurf <- wire3d.hxsurf

#' @method wire3d default
#' @export
wire3d.default <- function(x, ...) {
  stop("No wire3d method defined for objects of class: ", class(x))
}


#' @method wire3d plotlyshapelist3d
#' @export
wire3d.plotlyshapelist3d <- function (x, override = TRUE, ...) 
{
  invisible(unlist(sapply(x, function(item) wire3d(item, override = override, ...))))
}

#' @method wire3d plotlymesh3d
#' @export
wire3d.plotlymesh3d <- function(x, override = TRUE, ...) {
  
  psh <- openplotlyscene()$plotlyscenehandle
  params=list(...)
  material <- x$material
  opacity <- if("alpha" %in% names(params) && override == TRUE) {
                params$alpha } else if (!is.null(material$alpha)){
                  material$alpha 
                } else 1
  color <- if("col" %in% names(params) && override == TRUE)  {
              params$col } else if (!is.null(material$color)){
                material$color 
              } else 'black'
  width <- if("width" %in% names(params)) params$width else 2
  
  #Gather all edges for the faces..
  #Here vb is the points of the mesh, it is the faces of the mesh (this just has the order)..
  #To get the edges, just put the put the orders(faces) and collect the points represented by them..
  xyz=xyzmatrix(x)
  x_pts =  xyz[x$it, 1]
  y_pts =  xyz[x$it, 2]
  z_pts =  xyz[x$it, 3]
  
  ptsmat = cbind(x_pts,y_pts,z_pts)
  
  #Add na's after every three sets of points(as it is a triangle mesh)
  #The below fragment is to seperate them out into triangles..
  npts = 3 #triangle mesh
  
  #insert na's every idx pts..
  idx <- seq(from = npts+1, to = (npts+1)*nrow(ptsmat)/npts, by = npts+1)
  ptsna <- matrix(NA, nrow = (npts+1)*nrow(ptsmat)/npts, ncol=3) 
  idx_pts <- setdiff(1:nrow(ptsna),idx)
  ptsna[idx_pts,] = ptsmat
  
  psh <- psh %>% plotly::add_trace(type = 'scatter3d',
                                   x = ptsna[,1],
                                   y = ptsna[,2],
                                   z = ptsna[,3],
                                   mode = "lines",
                                   opacity = opacity,
                                   line = list(width = width, color = color))
  
  psh <- psh %>% plotly::layout(showlegend = FALSE, scene=list(camera=.plotly3d$camera))
  assign("plotlyscenehandle", psh, envir=.plotly3d)
  psh
}


#The below section is for adding the additional methods
#that are actually available in rgl for the wire3d class with the methods(nat::wire3d)

#' methods here imported from rgl
#' @method wire3d mesh3d
#' @inheritParams rgl::wire3d.mesh3d
#' @export 
wire3d.mesh3d <- utils::getFromNamespace("wire3d.mesh3d", "rgl")

#' methods here imported from rgl
#' @method wire3d shapelist3d
#' @param x object of type 'shapelist3d'
#' @param override Whether the material properties should override the ones in the shapes
#' @param ... Additional arguments passed to \code{\link[rgl]{wire3d}}
#' @export 
wire3d.shapelist3d <- utils::getFromNamespace("wire3d.shapelist3d", "rgl")


