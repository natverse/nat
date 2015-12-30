#' Plot neurons in 3D using rgl library
#' 
#' @export
#' @method plot3d neuron
#' @param x A neuron to plot
#' @param WithLine Whether to plot lines for all segments in neuron
#' @param NeuronNames Logical indicating whether to label the neuron in the plot
#'   using the NeuronName field \strong{or} a character vector of names.
#' @param WithNodes Whether to plot dots for branch and end points
#' @param WithAllPoints Whether to plot dots for all points in the neuron
#' @param WithText Whether to label plotted points with their numeric id (see 
#'   details)
#' @param PlotSubTrees Whether to plot all sub trees when the neuron is not 
#'   fully connected.
#' @param add Whether to add the neuron to existing rgl plot rather than 
#'   clearing the scene (default TRUE)
#' @param col Colour specification (see rgl materials)
#' @param soma Whether to plot a sphere at neuron's origin representing the 
#'   soma. Either a logical value or a numeric indicating the radius (default 
#'   \code{FALSE}). When \code{soma=TRUE} the radius is hard coded to 2.
#' @param ... Additional arguments passed to rgl::lines3d
#' @return list of rgl plotting ids (invisibly) separated into 
#'   \code{lines,points,texts} according to plot element. See 
#'   \code{\link[rgl]{plot3d}} for details.
#' @seealso \code{\link{plot3d.neuronlist}, \link{plot3d.dotprops}, 
#'   \link[rgl]{plot3d}}
#' @details Note that when WithText=TRUE, the numeric identifiers plotted are
#'   \emph{raw indices} into the \code{x$d} array, \emph{not} the values of the
#'   \code{PointNo} column.
#' @examples
#' # A new plot would have been opened if required
#' open3d()
#' plot3d(Cell07PNs[[1]],col='red')
#' plot3d(Cell07PNs[[2]],col='green')
#' # clear the current plot
#' plot3d(Cell07PNs[[2]],col='blue',add=FALSE)
#' # plot the number of all nodes
#' plot3d(Cell07PNs[[2]],col='red',WithText=TRUE,add=FALSE)
#' # include cell bodies
#' plot3d(Cell07PNs[3:4], col='red', soma=TRUE)
#' plot3d(Cell07PNs[5], col='red', soma=3)
#' rgl.close()
plot3d.neuron<-function(x, WithLine=TRUE, NeuronNames=FALSE, WithNodes=TRUE,
                        WithAllPoints=FALSE, WithText=FALSE, PlotSubTrees=TRUE,
                        add=TRUE, col=NULL, soma=FALSE, ...){
  if (!add)
    clear3d()
  # skip so that the scene is updated only once per neuron
  skip <- par3d(skipRedraw = TRUE)
  on.exit(par3d(skip))
  
  # Check colour setting col may not be properly set
  if(is.null(col)){
    # see if we have some materials to use
    Material=attr(x$SegmentProps,'Material')
    if(!is.null(Material)){
      rownames(Material)=Material$id
      Ids<-do.call(c,sapply(x$SegList,function(s) {c(x$d[s,'Label'],NA)},simplify=FALSE))
      col=Material[as.character(Ids),'col']
    } 
    else col='green'
    # otherwise just plot in default colour
  }
  if(is.function(col)) col=col(1)
  
  rglreturnlist=list()
  NodesOnly<-c(x$BranchPoints,
               x$EndPoints[-which(x$EndPoints==x$StartPoint)],
               x$StartPoint)
  NodeCols<-c(rep("red",length(x$BranchPoints)),
              rep("blue",length(x$EndPoints)-1),"purple" )
  
  if(WithNodes){
    if(WithAllPoints){
      SimplePoints=setdiff(seq.int(length.out = nrow(x$d)), NodesOnly)
      AllPoints=c(NodesOnly, SimplePoints)
      NodeCols=c(NodeCols, rep(col, length(SimplePoints)))
      rglreturnlist[["points"]]=points3d(x$d[AllPoints, 
                                             c("X","Y","Z")], color=NodeCols, size=3)
      if(WithText) # text labels for nodes
        rglreturnlist[["texts"]]=texts3d(x$d[AllPoints, c("X","Y","Z")],
                                         texts=AllPoints, color=NodeCols, adj=c(0,0.5))
    } else {
      if(!WithLine) NodeCols=col
      rglreturnlist[["points"]]=points3d(x$d[NodesOnly,c("X","Y","Z")],color=NodeCols,size=3)
      if(WithText) # text labels for nodes
        rglreturnlist[["texts"]]=texts3d(x$d[NodesOnly,c("X","Y","Z")],texts=NodesOnly,color=NodeCols,adj=c(0,0.5))
    }
  }
  
  if(PlotSubTrees && !is.null(x$SubTrees)) x$SegList=unlist(x$SubTrees,recursive=FALSE)
  
  d=data.matrix(x$d[,c("X","Y","Z")])
  # xyzl=sapply(x$SegList,function(s) {rbind(d[s,],NA)})
  # NAs are used to break line segments
  if(WithLine){
    xyzl<-do.call(rbind,sapply(x$SegList,function(s) {rbind(d[s,],NA)},simplify=FALSE))
    rglreturnlist[["lines"]]=lines3d(xyzl,col=col,...)
  }
  
  if(is.logical(NeuronNames) && NeuronNames) NeuronNames=x$NeuronName
  if(!is.logical(NeuronNames)){
    StartPoint=ifelse(is.null(x$StartPoint),1,x$StartPoint)
    rglreturnlist[["texts"]]=texts3d(d[StartPoint,],texts=NeuronNames,col=col)
  }
  
  somarad=2
  if(is.numeric(soma)) {
    somarad=soma
    soma=TRUE
  }
  
  if(soma && !is.null(x$StartPoint)){
    somapos=x$d[x$StartPoint,c("X", "Y", "Z")]
    rglreturnlist[['soma']] <- spheres3d(somapos, radius = somarad, col = col)
  }
  
  invisible(rglreturnlist)
}

#' plot3d methods for different nat objects
#' 
#' These methods enable nat objects including neuronlists and dotprops objects
#' to be plotted in 3D. See the help for each individual method for details along
#' with the help for the generic in the rgl package.
#' 
#' @name plot3d
#' @examples 
#' ## up to date list of all plot3d nethods in this package
#' intersect(methods("plot3d"), ls(envir = as.environment('package:nat')))
#' @seealso \code{\link[rgl]{plot3d}}, \code{\link{plot3d.boundingbox}},
#'   \code{\link{plot3d.character}}, \code{\link{plot3d.dotprops}},
#'   \code{\link{plot3d.hxsurf}}, \code{\link{plot3d.neuron}},
#'   \code{\link{plot3d.neuronlist}}
NULL

#' Open customised rgl window
#' 
#' Pan with right button (Ctrl+click), zoom with middle (Alt/Meta+click) button.
#' Defaults to a white background and orthogonal projection (FOV=0)
#' 
#' Note that sometimes (parts of) objects seem to disappear after panning and
#' zooming. See help for \code{\link{pan3d}}.
#' @param bgcol background colour
#' @param FOV field of view
#' @param ... additional options passed to open3d
#' @return current rgl device
#' @export
#' @seealso \code{\link{open3d},\link{pan3d}}
nopen3d<- function(bgcol='white', FOV=0, ...){
  res=open3d(mouseMode=c("trackball","user","zoom"), FOV=FOV, ...)
  bg3d(col=bgcol)
  pan3d(2)
  res
}

#' Some useful extensions / changes to rgl defaults
#' 
#' Set up pan call back for current rgl device
#' 
#' Copied verbatim from ?rgl.setMouseCallbacks for rgl version 0.92.892 Mouse
#' button 2 is right and button 3 is middle (accessed by meta/alt key)
#' 
#' Note that sometimes (parts of) objects seem to disappear after panning and 
#' zooming. The example in \code{\link{rgl.setMouseCallbacks}} from which this
#' is copied includes a note that "this doesn't play well with rescaling"
#' @param button Integer from 1 to 3 indicating mouse button
#' @seealso \code{\link{rgl.setMouseCallbacks}}
#' @author Duncan Murdoch
#' @examples
#' \dontrun{
#'  open3d()
#'  pan3d(2)
#' }
pan3d <- function(button) {
  start <- list()
  begin <- function(x, y) {
    start$userMatrix <<- par3d("userMatrix")
    start$viewport <<- par3d("viewport")
    start$scale <<- par3d("scale")
    start$projection <<- rgl.projection()
    start$pos <<- rgl.window2user( x/start$viewport[3], 1 - y/start$viewport[4],
                                   0.5, projection=start$projection)
  }
  update <- function(x, y) {
    xlat <- (rgl.window2user( x/start$viewport[3], 1 - y/start$viewport[4],
                              0.5, projection = start$projection) - start$pos)*start$scale
    mouseMatrix <- translationMatrix(xlat[1], xlat[2], xlat[3])
    par3d(userMatrix = start$userMatrix %*% t(mouseMatrix) )
  }
  rgl.setMouseCallbacks(button, begin, update)
}

#' Plot a 2D projection of a neuron
#' 
#' @export
#' @details This functions sets the axis ranges based on the chosen
#'   \code{PlotAxes} and the range of the data in \code{x}. It is still possible
#'   to use \code{PlotAxes} in combination with a \code{boundingbox}, for
#'   example to set the range of a plot of a number of objects.
#'   
#'   nat assumes the default axis convention used in biological imaging, where
#'   the origin of the y axis is the top rather than the bottom of the plot.
#'   This is achieved by reversing the y axis of the 2D plot when the second
#'   data axis is the Y axis of the 3D data. Other settings can be achieved by
#'   modfiying the AxisDirections argument.
#'   
#' @param x a neuron to plot.
#' @param WithLine whether to plot lines for all segments in neuron.
#' @param WithNodes whether points should only be drawn for nodes (branch/end 
#'   points)
#' @param WithAllPoints whether points should be drawn for all points in neuron.
#' @param WithText whether to label plotted points with their id.
#' @param soma Whether to plot a circle at neuron's origin representing the 
#'   soma. Either a logical value or a numeric indicating the radius (default 
#'   \code{FALSE}). When \code{soma=TRUE} the radius is hard coded to 2.
#' @param PlotAxes the axes for the plot.
#' @param axes whether axes should be drawn.
#' @param asp the \code{y/x} aspect ratio, see \code{\link{plot.window}}.
#' @param main the title for the plot
#' @param sub sub title for the plot
#' @param xlim limits for the horizontal axis (see also boundingbox)
#' @param ylim limits for the vertical axis (see also boundingbox)
#' @param AxisDirections the directions for the axes. By default, R uses the 
#'   bottom-left for the origin, whilst most graphics software uses the 
#'   top-left. The default value of \code{c(1, -1, 1)} makes the produced plot 
#'   consistent with the latter.
#' @param add Whether the plot should be superimposed on one already present 
#'   (default: \code{FALSE}).
#' @param col the color in which to draw the lines between nodes.
#' @param PointAlpha the value of alpha to use in plotting the nodes.
#' @param tck length of tick mark as fraction of plotting region (negative 
#'   number is outside graph, positive number is inside, 0 suppresses ticks, 1 
#'   creates gridlines).
#' @param lwd line width relative to the default (default=1).
#' @param boundingbox A 2 x 3 matrix (ideally of class 
#'   \code{\link{boundingbox}}) that enables the plot axis limits to be set 
#'   without worrying about axis selection or reversal (see details)
#' @param ... additional arguments passed to plot
#' @return list of plotted points (invisibly)
#' @seealso \code{\link{plot3d.neuron}}
#' @examples
#' # Draw first example neuron
#' plot(Cell07PNs[[1]])
#' # Overlay second example neuron
#' plot(Cell07PNs[[2]], add=TRUE)
#' # Clear the current plot and draw the third neuron from a different view
#' plot(Cell07PNs[[3]], PlotAxes="YZ")
#' # Just plot the end points for the fourth example neuron
#' plot(Cell07PNs[[4]], WithNodes=FALSE)
#' # Plot with soma (of default radius)
#' plot(Cell07PNs[[4]], WithNodes=FALSE, soma=TRUE)
#' # Plot with soma of defined radius
#' plot(Cell07PNs[[4]], WithNodes=FALSE, soma=1.25)
plot.neuron <- function(x, WithLine=TRUE, WithNodes=TRUE, WithAllPoints=FALSE,
                        WithText=FALSE, soma=FALSE,
                        PlotAxes=c("XY", "YZ", "XZ", "ZY"), axes=TRUE, asp=1,
                        main=x$NeuronName, sub=NULL, xlim=NULL, ylim=NULL,
                        AxisDirections=c(1,-1,1), add=FALSE, col=NULL,
                        PointAlpha=1, tck=NA, lwd=par("lwd"), 
                        boundingbox=NULL, ...) {
  
  # R uses the bottom-left as the origin, while we want the top-left
  PlotAxes <- match.arg(PlotAxes)
  if(PlotAxes=="XY") {PlotAxes<-c("X","Y");NumPlotAxes<-c(1,2)} else
    if(PlotAxes=="YZ") {PlotAxes<-c("Y","Z");NumPlotAxes<-c(2,3)} else
      if(PlotAxes=="XZ") {PlotAxes<-c("X","Z");NumPlotAxes<-c(1,3)} else 
        if(PlotAxes=="ZY") {PlotAxes<-c("Z","Y");NumPlotAxes<-c(3,2)}
  
  if(WithAllPoints){
    mycols<-rep("red",x$NumPoints)
    mycols[x$BranchPoints]<-"red"
    mycols[x$EndPoints]<-"green"
    mycols[x$StartPoint]<-"purple"
    PlottedPoints<-x$d[,c("PointNo",PlotAxes)]
  } else if(WithNodes){
    NodesOnly<-c(x$BranchPoints,x$EndPoints,x$StartPoint)
    mycols<-c(rep(rgb(1,0,0,PointAlpha),length(x$BranchPoints)),
              rep(rgb(0,1,0,PointAlpha),length(x$EndPoints)),
              rgb(t(col2rgb('purple')/255),alpha=PointAlpha) )
    PlottedPoints<-x$d[NodesOnly,c("PointNo",PlotAxes)]
  } else {
    PlottedPoints <- x$d[integer(),c("PointNo",PlotAxes)]
    mycols<-NULL
  }
  
  # Draw the plot
  if(add) points(PlottedPoints[,PlotAxes],col=mycols,pch=20,asp=asp,...)
  else {
    # We are setting up the plot, so we need to find limits for axes 
    # (inverting y axis if necessary due to differing handedness)
    if(is.null(boundingbox))
      boundingbox=boundingbox(x, na.rm=TRUE)
    colnames(boundingbox)=c("X","Y","Z")
    myxlims <- boundingbox[,PlotAxes[1]]
    myylims <- boundingbox[,PlotAxes[2]]
    
    AxesToReverse=c("X","Y","Z")[AxisDirections<0]
    if(PlotAxes[2] %in% AxesToReverse) myylims <- rev(myylims)
    
    if (!is.null(xlim)) {
      myxlims=xlim
    }
    if (!is.null(ylim)) {
      myylims=ylim
    }
    
    plot(PlottedPoints[,PlotAxes],col=mycols,pch=20,xlim=myxlims,ylim=myylims,
            main=main,sub=sub,asp=asp,axes=F,tck=tck,...)
    # Draw the axes and surrounding box
    if(axes) {
      box()
      axis(2, tck=tck)
      axis(1, tck=tck)
    }
  }
  
  if(WithText && !is.null(PlottedPoints)) {
    text(PlottedPoints[,PlotAxes[1]],PlottedPoints[,PlotAxes[2]],
         PlottedPoints[,"PointNo"],col=mycols,pos=3)
  }
  
  if (WithLine) {
    if(!is.null(col) && length(col==1)) {
      MyCols=rep(col,x$NumSegs)
    } else {
      MyCols<-rep(1,x$NumSegs)
      if(!is.null(col)) {
        MyCols=col[MyCols]
      }
    }
    for(thisCol in unique(MyCols)) {
      SegsToPlot=x$SegList[MyCols==thisCol]
      LinesToPlot=unlist(sapply(SegsToPlot,function(x) c(x,NA)))	
      lines(x$d[LinesToPlot,PlotAxes],col=thisCol,lwd=lwd)
    }
  }
  
  somarad=2
  if(is.numeric(soma)) {
    somarad=soma
    soma=TRUE
  }
  if(soma){
    somapos=x$d[x$StartPoint,PlotAxes]
    symbols(somapos[[1]], somapos[[2]], circles = somarad, inches = F, add=T,
            bg=ifelse(is.null(col[1]), 1, col[1]), fg=NA)
  }
  invisible(PlottedPoints)
}


#' Plot a bounding box in 3D
#' 
#' @param x the \code{\link{boundingbox}} object to plot.
#' @param ... additional arguments to pass to \code{\link[rgl]{segments3d}}.
#' @return A list of RGL object IDs.
#'   
#' @method plot3d boundingbox
#' @export
#' @seealso \code{\link{boundingbox}}
#' @examples
#' # plot some neurons
#' clear3d()
#' # NB skipRedraw draws all neurons in one go
#' plot3d(kcs20, skipRedraw = TRUE)
#' # plot the bounding box of all the neurons
#' plot3d(boundingbox(kcs20))
#' 
#' \dontrun{
#' plot3d(kcs20)
#' # plot bounding box (in matching colours) for each neuron
#' # NB makes use of nlapply/neuronlist in slightly unsusual context - 
#' # plot3d.neuronlist can cope with lists containing anything with
#' # a valid plot3d method.
#' plot3d(nlapply(kcs20,boundingbox))
#' }
#' 
plot3d.boundingbox <- function(x, ...) {
  pts <- matrix(c(
  c(x[1, 1], x[1, 2], x[1, 3]),
  c(x[1, 1], x[1, 2], x[2, 3]),
  c(x[1, 1], x[2, 2], x[1, 3]),
  c(x[1, 1], x[2, 2], x[2, 3]),
  c(x[2, 1], x[1, 2], x[1, 3]),
  c(x[2, 1], x[1, 2], x[2, 3]),
  c(x[2, 1], x[2, 2], x[1, 3]),
  c(x[2, 1], x[2, 2], x[2, 3])
  ), ncol=3, byrow=TRUE)
  segments3d(pts[c(1:8, 1, 3, 5, 7, 2, 4, 1, 5, 2, 6, 3, 7, 4, 8, 6, 8), ], ...)
}
