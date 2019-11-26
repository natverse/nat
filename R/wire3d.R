#' Wire frame plots 
#' 
#' This function directs the wireframe plot based on the plotengine backend selected.
#' @param x mesh object of type 'mesh3d' (which should be a triangular mesh)
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
wire3d <- function(x, plotengine = getOption('nat.plotengine'), ...) {
  if(inherits(x, 'mesh3d')){
    
    tmesh <- x
    
    if(plotengine == 'plotly') {
      psh <- openplotlyscene()$plotlyscenehandle
      params=list(...)
      opacity <- if("alpha" %in% names(params)) params$alpha else 1
      color <- if("col" %in% names(params)) params$col else 'black'
      width <- if("width" %in% names(params)) params$width else 2
      
      #Gather all edges for the faces..
      #Here vb is the points of the mesh, it is the faces of the mesh (this just has the order)..
      #To get the edges, just put the put the orders(faces) and collect the points represented by them..
      x_pts =  tmesh$vb[1, tmesh$it]
      y_pts =  tmesh$vb[2, tmesh$it]
      z_pts =  tmesh$vb[3, tmesh$it]
      
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
    }
    
    # Just retain the previous behaviour
    if(plotengine == 'rgl') { rglreturnlist <- rgl::wire3d(tmesh, ...)}
    
    if (plotengine == 'rgl'){
      invisible(rglreturnlist)
    } else{
      psh <- psh %>% 
        plotly::layout(showlegend = FALSE, scene=list(camera=.plotly3d$camera))
      assign("plotlyscenehandle", psh, envir=.plotly3d)
      psh
    }
    
  }
}
