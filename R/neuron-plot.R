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
#' rgl.close()
plot3d.neuron<-function(x, WithLine=TRUE, NeuronNames=FALSE, WithNodes=TRUE,
                        WithAllPoints=FALSE, WithText=FALSE, PlotSubTrees=TRUE,
                        add=TRUE, col=NULL,...){
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
  
  invisible(rglreturnlist)
}
