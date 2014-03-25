#' Plot neurons in 3d using rgl library
#' 
#' @export
#' @method plot3d neuron
#' @param x A neuron to plot
#' @param WithLine Whether to plot lines for all segments in neuron
#' @param NeuronNames Logical indicating whether to label the neuron in the plot
#'   using the NeuronName field \strong{or} a character vector of names.
#' @param WithNodes Whether to plot dots for branch and end points
#' @param WithAllPoints Whether to plot dots for all points in the neuron
#' @param WithText Whether to label plotted points with their id
#' @param PlotSubTrees Whether to plot all sub trees when the neuron is not 
#'   fully connected.
#' @param add Whether to add the neuron to existing rgl plot rather than 
#'   clearing the scene (default TRUE)
#' @param col Colour specification (see rgl materials)
#' @param soma Whether to plot a sphere at neuron's origin representing the
#'   soma. Either a logical value or a numeric indicating the radius.
#' @param ... Additional arguments passed to rgl::lines3d
#' @return list of rgl plotting ids (invisibly) separated into 
#'   \code{lines,points,texts} according to plot element. See 
#'   \code{\link[rgl]{plot3d}} for details.
#' @seealso \code{\link{plot3d.neuronlist}, \link{plot3d.dotprops}, 
#'   \link[rgl]{plot3d}}
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
              rep("green",length(x$EndPoints)-1),"purple" )
  
  if(WithNodes){
    Colour=col
    if(WithAllPoints){
      if(!WithLine) NodeCols=rep(Colour,nrow(x$d))
      rglreturnlist[["points"]]=points3d(x$d[,c("X","Y","Z")],color=NodeCols,size=3)
      if(WithText) # text labels for nodes
        rglreturnlist[["texts"]]=texts3d(x$d[,c("X","Y","Z")],texts=seq(nrow(x$d)),color=NodeCols,adj=c(0,0.5))
    } else {
      if(!WithLine) NodeCols=rep(Colour,length(NodeCols))
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

#' Open customised rgl window
#'
#' Pan with right button (Ctrl+click), zoom with middle
#'   (Alt/Meta+click) button. Defaults to a white background and orthogonal
#'   projection (FOV=0)
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
#' Copied verbatim from ?rgl.setMouseCallbacks for rgl version 0.92.892
#' Mouse button 2 is right and button 3 is middle (accessed by meta/alt key)
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

#' Plot a 2D project of a neuron
#' 
#' @export
#' @method plot neuron
#' @param x a neuron to plot.
#' @param WithLine whether to plot lines for all segments in neuron.
#' @param NodesOnly whether points should only be drawn for nodes, not all
#'   points in neuron.
#' @param EPsOnly whether points should only be drawn for end points.
#' @param BPsOnly whether points should only be drawn for branch points.
#' @param WithText whether to label plotted points with their id.
#' @param UseCurPalette whether the current palette should be used for colors.
#' @param PlotAxes the axes for the plot.
#' @param Axes whether axes should be drawn.
#' @param asp the \code{y/x} aspect ratio, see \code{\link{plot.window}}.
#' @param MainTitle the title for the plot.
#' @param xlim limits for the horizontal axis.
#' @param ylim limits for the vertical axis.
#' @param AxisDirections the directions for the axes. By default, R uses the
#'   bottom-left for the origin, whilst most graphics software uses the
#'   top-left. The default value of \code{c(1, -1, 1)} makes the produced plot
#'   consistent with the latter.
#' @param Superimpose whether the plot should be superimposed on one already
#'   present.
#' @param LineCol the color in which to draw the lines between nodes.
#' @param PointAlpha the value of alpha to use in plotting the nodes.
#' @param tck length of tick mark as fraction of plotting region (negative
#'   number is outside graph, positive number is inside, 0 suppresses ticks, 1
#'   creates gridlines).
#' @param lwd line width relative to the default (default=1).
#' @param ... additional arguments passed to plot
#' @return list of plotted points (invisibly)
#' @seealso \code{\link{plot3d.neuron}}
#' @examples
#' # Draw first example neuron
#' plot(Cell07PNs[[1]])
#' # Overlay second example neuron
#' plot(Cell07PNs[[2]], Superimpose=TRUE)
#' # Clear the current plot and draw the third neuron from a different view
#' plot(Cell07PNs[[3]], PlotAxes="YZ")
#' # Just plot the end points for the fourth example neuron
#' plot(Cell07PNs[[4]], EPsOnly=TRUE)
plot.neuron <- function(x, WithLine=TRUE, NodesOnly=TRUE, EPsOnly=FALSE,
                        BPsOnly=FALSE, WithText=FALSE, UseCurPalette=FALSE,
                        PlotAxes=c("XY", "YZ", "XZ", "ZY"), Axes=TRUE, asp=1,
                        MainTitle=x$NeuronName, xlim=NULL, ylim=NULL,
                        AxisDirections=c(1,-1,1), Superimpose=F, LineCol=NULL,
                        PointAlpha=1, tck=NA, lwd=par("lwd"), ...) {
  
  # R uses the bottom-left as the origin, while we want the top-left
  if(any(AxisDirections!=1)) {
    x$d[,c("X","Y","Z")]=t(t(x$d[,c("X","Y","Z")])*AxisDirections)
  }
  PlotAxes <- match.arg(PlotAxes)
  if(PlotAxes=="XY") {PlotAxes<-c("X","Y");NumPlotAxes<-c(1,2)} else
    if(PlotAxes=="YZ") {PlotAxes<-c("Y","Z");NumPlotAxes<-c(2,3)} else
      if(PlotAxes=="XZ") {PlotAxes<-c("X","Z");NumPlotAxes<-c(1,3)} else 
        if(PlotAxes=="ZY") {PlotAxes<-c("Z","Y");NumPlotAxes<-c(3,2)}
  
  OldPalette<-palette()
  if(!UseCurPalette) {
    palette(c("black",rainbow(6)))
  }
    
  # Set limits for axes
  myxlims<-range(x$d[PlotAxes[1]],na.rm = TRUE)
  myylims<-range(x$d[PlotAxes[2]],na.rm = TRUE)
  if (!is.null(xlim)) {
    myxlims=xlim
  }
  if (!is.null(ylim)) {
    myylims=ylim
  }    
  
  if(EPsOnly) {
    NodesOnly=setdiff(x$EndPoints,x$StartPoint)
    mycols<-rep(rgb(0,1,0,PointAlpha),length(x$EndPoints))
    PlottedPoints<-x$d[NodesOnly,c("PointNo",PlotAxes)]
  } else if(BPsOnly) {
    NodesOnly<-x$BranchPoints
    mycols<-rep("red",length(x$BranchPoints))
    PlottedPoints<-x$d[NodesOnly,c("PointNo",PlotAxes)]
  } else if(NodesOnly) {
    NodesOnly<-c(x$BranchPoints,x$EndPoints,x$StartPoint)
    mycols<-c(rep("red",length(x$BranchPoints)),
              rep("green",length(x$EndPoints)),"purple" )
    PlottedPoints<-x$d[NodesOnly,c("PointNo",PlotAxes)]
  } else {
    mycols<-rep("black",x$NumPoints)
    mycols[x$BranchPoints]<-"red"
    mycols[x$EndPoints]<-"green"
    mycols[x$StartPoint]<-"purple"
    PlottedPoints<-x$d[,c("PointNo",PlotAxes)]
  }
  
  # Draw the plot
  if(Superimpose) points(PlottedPoints[,PlotAxes],col=mycols,pch=20,asp=asp,...) 
  else plot(PlottedPoints[,PlotAxes],col=mycols,pch=20,xlim=myxlims,ylim=myylims,
            main=MainTitle,asp=asp,axes=Axes && all(AxisDirections==1),tck=tck,...) 
  if(Axes && !all(AxisDirections==1) && !Superimpose) {
    # Need to provide special treatment for axes
    box()
    if(AxisDirections[NumPlotAxes][1]!=1) {
      axis(1, at=axTicks(1),tck=tck,
           labels=axTicks(1)*AxisDirections[NumPlotAxes][1])
    } else {
      axis(tck=tck,1)
    }
    if(AxisDirections[NumPlotAxes][2]!=1) {
      axis(2, at=axTicks(2),tck=tck,
           labels=axTicks(2)*AxisDirections[NumPlotAxes][2])
    } else {
      axis(tck=tck,2)
    }
  }
  
  if(WithText) {
    text(PlottedPoints[,PlotAxes[1]],PlottedPoints[,PlotAxes[2]],
         PlottedPoints[,"PointNo"],col=mycols,pos=3)
  }
  
  if (WithLine) {
    if(!is.null(LineCol) && length(LineCol==1)) {
      MyCols=rep(LineCol,x$NumSegs)
    } else {
      MyCols<-rep(1,x$NumSegs)
      if(!is.null(LineCol)) {
        MyCols=LineCol[MyCols]
      }
    }
    for(thisCol in unique(MyCols)) {
      SegsToPlot=x$SegList[MyCols==thisCol]
      LinesToPlot=unlist(sapply(SegsToPlot,function(x) c(x,NA)))	
      lines(x$d[LinesToPlot,PlotAxes],col=thisCol,lwd=lwd)
    }
  }
  
  if(!is.null(x$c$GrandCent)) {
    points(x$c$GrandCent[NumPlotAxes[1]],
           x$c$GrandCent[NumPlotAxes[2]],col="red",bg="red",pch=22)
  }
  
  palette(OldPalette)
  invisible(PlottedPoints)
}
