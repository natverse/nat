plotneuron3d.simple<-function(ANeuron, WithLine=T,NeuronNames=FALSE,
                              WithNodes=T,WithAllPoints=F,WithText=F,HighlightLongestSegment=FALSE,PlotSubTrees=T,ClearRGL=T,NeuronList=MyNeurons,col,...){
  # rewrite of plotneuron3d using updated rgl calls
  
  require(rgl)
  r3dDefaults$bg<-'grey'
  if(ClearRGL) clear3d()
  
  if ( is.character(ANeuron) || is.numeric(ANeuron) ){
    if(length(ANeuron)>1){
      if(missing(col)) col=rainbow
      if(is.function(col)) col=col(length(ANeuron))
      if(is.factor(col)) col=rainbow(nlevels(col))[as.integer(col)]
      if(is.logical(NeuronNames) && NeuronNames) NeuronNames=ANeuron
      return(invisible(mapply(plotneuron3d.simple,
                              NeuronList[ANeuron],col=col,ClearRGL=FALSE,NeuronNames=NeuronNames,
                              WithNodes=WithNodes,WithText=WithText,WithLine=WithLine,
                              HighlightLongestSegment=HighlightLongestSegment,PlotSubTrees=PlotSubTrees,...)))
    } else ANeuron<-NeuronList[[ANeuron]]
  }
  
  if (!is.neuron(ANeuron)){
    warning("Cannot understand passed neuron")
    return(F)
  }
  # at this point we've only got one neuron, but col may not be properly set
  if(missing(col)){
    # see if we have some materials to use
    Material=attr(ANeuron$SegmentProps,'Material')
    if(!is.null(Material)){
      rownames(Material)=Material$id
      Ids<-do.call(c,sapply(ANeuron$SegList,function(s) {c(ANeuron$d[s,'Label'],NA)},simplify=FALSE))
      col=Material[as.character(Ids),'col']
    } 
    else col='green'
    # otherwise just plot in default colour
  }
  if(is.function(col)) col=col(1)
  
  rglreturnlist=list()
  NodesOnly<-c(ANeuron$BranchPoints,
               ANeuron$EndPoints[-which(ANeuron$EndPoints==ANeuron$StartPoint)],
               ANeuron$StartPoint)
  NodeCols<-c(rep("red",length(ANeuron$BranchPoints)),
              rep("green",length(ANeuron$EndPoints)-1),"purple" )
  
  if(WithNodes){
    Colour=col
    if(WithAllPoints){
      if(!WithLine) NodeCols=rep(Colour,nrow(ANeuron$d))
      rglreturnlist[["points"]]=points3d(ANeuron$d[,c("X","Y","Z")],color=NodeCols,size=3)
      if(WithText) # text labels for nodes
        rglreturnlist[["texts"]]=texts3d(ANeuron$d[,c("X","Y","Z")],text=seq(nrow(ANeuron$d)),color=NodeCols,adj=c(0,0.5))
    } else {
      if(!WithLine) NodeCols=rep(Colour,length(NodeCols))
      rglreturnlist[["points"]]=points3d(ANeuron$d[NodesOnly,c("X","Y","Z")],color=NodeCols,size=3)
      if(WithText) # text labels for nodes
        rglreturnlist[["texts"]]=texts3d(ANeuron$d[NodesOnly,c("X","Y","Z")],text=NodesOnly,color=NodeCols,adj=c(0,0.5))
    }
  }
  
  if(PlotSubTrees && !is.null(ANeuron$SubTrees)) ANeuron$SegList=unlist(ANeuron$SubTrees,recursive=FALSE)
  
  d=data.matrix(ANeuron$d[,c("X","Y","Z")])
  # xyzl=sapply(ANeuron$SegList,function(s) {rbind(d[s,],NA)})
  # NAs are used to break line segments
  if(WithLine){
    xyzl<-do.call(rbind,sapply(ANeuron$SegList,function(s) {rbind(d[s,],NA)},simplify=FALSE))
    rglreturnlist[["lines"]]=lines3d(xyzl,col=col,...)
    if(HighlightLongestSegment){
      x=GetLongestSegment(ANeuron)
      rglreturnlist[["spheres"]]=spheres3d(x,col=col,...)
    }
  }
  
  if(is.logical(NeuronNames) && NeuronNames) NeuronNames=ANeuron$NeuronName
  if(!is.logical(NeuronNames)){
    StartPoint=ifelse(is.null(ANeuron$StartPoint),1,ANeuron$StartPoint)
    texts3d(d[StartPoint,],texts=NeuronNames,col=col)
  }
  
  invisible(rglreturnlist)
}
