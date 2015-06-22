# read neurons in the format produced by Amira skeletonize plugin
#' @importFrom igraph as.undirected
read.neuron.hxskel<-function(file, ...){
  ndata=read.amiramesh(file)
  required_fields=c("Coordinates", "NeighbourCount", "Radii", "NeighbourList")
  missing_fields=setdiff(required_fields,names(ndata))
  if(length(missing_fields))
    stop("Neuron: ",file," is missing fields: ",paste(missing_fields,collapse=" "))
  
  d=data.frame(ndata$Coordinates)
  colnames(d)=c("X","Y","Z")
  d$W=ndata$Radii*2
  nVertices=nrow(d)
  d$PointNo=seq(nVertices)
  d[,1:4]=zapsmall(d[,1:4])
  # Note these numbers come in zero indexed, but I will want them 1-indexed
  Neighbours=data.frame(Neighbour=ndata$NeighbourList+1,
                        CurPoint=rep(seq(nVertices),ndata$NeighbourCount))
  Origin=ndata$Origins
  if(!is.null(Origin)) Origin=Origin+1
  
  # Handle materials
  if(length(ndata$vertexTypeList)){
    SegmentProps=data.frame(
      PointNo=rep(seq(nVertices),ndata$vertexTypeCounter),
      Id=ndata$vertexTypeList)
    if(length(attr(ndata,'Materials'))){
      # we have named materials, so let's add them as an attribute
      attr(SegmentProps,'Materials')=attr(ndata,'Materials')
    }
    # we can only add one numeric label to the SWC format version of the neuron
    FirstSegmentProps=SegmentProps[!duplicated(SegmentProps$PointNo),]
    # 0 = undefined
    d$Label=0L
    d[FirstSegmentProps$PointNo,"Label"]=FirstSegmentProps$Id
  }
  
  el=data.matrix(Neighbours)
  # make a doubly linked graph from this double edge list
  doubleg=ngraph(el, d$PointNo, directed=TRUE)
  # TODO see if we can make appropriate directed graph rather than converting
  # to undirected.
  ug=as.undirected(doubleg, mode='collapse')
  if(!inherits(ug,'ngraph')) class(ug)=c("ngraph",class(ug))
  n=as.neuron(ug, vertexData=d, origin=Origin, InputFileName=file, ... )
  n
}

is.hxskel<-is.amiratype('SkeletonGraph')

# Read neuron in Amira's native lineset format
# @param file Path to the amiramesh file
# @param defaultDiameter If diameter information, missing use this default
# @return A neuron object
read.neuron.hxlineset<-function(file, defaultDiameter=NA_real_, ...){
  amdata=read.amiramesh(file)
  if(!all(c("Coordinates","LineIdx")%in%names(amdata)))
    stop("Cannot find required data sections")
  
  coords=as.data.frame(amdata$Coordinates)
  colnames(coords)=c("X","Y","Z")
  
  # See if we can find some radius data in one of the other data sections
  radiusData=amdata[!names(amdata)%in%c("Coordinates","LineIdx")]
  lad=length(radiusData)
  if(lad==0){
    warning("No width data for neuron:",file)
    coords[,"W"]=defaultDiameter
  } else if (lad==1) {
    # assume Amira provides radius
    coords[,"W"]=radiusData[[1]]*2
  } else if (lad>1) {
    warning("Assuming that Data section ",lad," (",names(radiusData)[lad],") specifies radius")
    coords[,"W"]=radiusData[[lad]]*2
  }
  
  coords=cbind(PointNo=seq(1:nrow(coords)), zapsmall(coords))
  
  # extract points that define lines (and immediately convert to 1-indexed)
  lpts = amdata$LineIdx+1
  lpts[lpts==0]=NA
  # construct edge list
  el=cbind(start=lpts[-length(lpts)], end=lpts[-1])
  el=el[!is.na(rowSums(el)),]
  ng=ngraph(el, vertexlabels=coords$PointNo, xyz = coords[,c("X","Y","Z"), drop=FALSE], diam=coords[,"W"])
  as.neuron(ng, InputFileName=file, ...)
}

is.hxlineset<-is.amiratype('HxLineSet')

# write out a neuron in the specialised skeletonize AM3D format 
# (as opposed to the basic AmiraMesh format which is the native format
# of amira for linesets)
# WriteAllSubTrees will write out all the stored subtrees in a neuron 
# which has multiple subtrees (which is often true of ill-formed 
# skeletonize neurons).  It will also add a data field that can be used
# to visualised different subtrees eg by colouring
write.neuron.hxskel<-function(x, file, WriteAllSubTrees=TRUE, 
                              ScaleSubTreeNumsTo1=TRUE, sep=NULL){
  # if asked & nTrees is >1  (NB isTRUE handles NULL case correctly)
  if(WriteAllSubTrees && isTRUE(x$nTrees>1)){	
    WriteAllSubTrees=TRUE 
    # nb recurs =F, so list of lists -> list (rather than vector)
    SegList=unlist(x$SubTrees,recursive=FALSE)
  } else {
    WriteAllSubTrees=FALSE
    SegList=x$SegList
  }
  chosenVertices=sort(unique(unlist(SegList)))
  nVertices=length(chosenVertices)
  # I think that restristicting to chosen vertices without
  # any renumbering of points is problematic
  # FIXME - need to renumber vertices if there are NAs
  # nVertices=nrow(x$d)
  # the number of points required to define the edge list
  # if each segment contains n points, then 2(n-1) edges
  nEdgeList=sum(sapply(SegList,length)-1)*2
  # Make EdgeList
  makeEdges=function(seg){
    lSeg=length(seg)
    if(lSeg<2) return()
    elFwd=cbind(seg[-lSeg],seg[-1])
    elRev=cbind(seg[-1],seg[-lSeg])
    df=data.frame(rbind(elFwd,elRev))
    names(df)=c("CurPoint","Neighbour")
    df[order(df$CurPoint,df$Neighbour),]
  }
  EdgeList=do.call('rbind',lapply(SegList,makeEdges))
  EdgeList=EdgeList[order(EdgeList$CurPoint,EdgeList$Neighbour),]
  
  # Write the header
  cat("# AmiraMesh 3D ASCII 2.0\n",file=file)
  fc=file(file,open="at") # ie append, text mode
  
  cat("# Created by nat::write.neuron.hxskel\n\n",file=fc)
  cat("nVertices", nVertices,"\nnEdges",nEdgeList,"\n",file=fc)
  
  vertexTypeList=ifelse(WriteAllSubTrees,nVertices,0) 
  cat("define Origins 1\ndefine vertexTypeList",vertexTypeList,"\n\n",file=fc)
  
  cat("Parameters {\n",file=fc)
  cat("    ContentType \"SkeletonGraph\"\n",file=fc)
  cat("}\n\n",file=fc)
  
  cat("Vertices { float[3] Coordinates } @1\n",file=fc)
  cat("Vertices { int NeighbourCount } @2\n",file=fc)
  cat("Vertices { float Radii } @3\n",file=fc)
  cat("EdgeData { int NeighbourList } @4\n",file=fc)
  cat("Origins { int Origins } @5\n",file=fc)
  cat("Vertices { int vertexTypeCounter } @6\n",file=fc)
  cat("vertexTypeList { int vertexTypeList } @7\n\n",file=fc)
  
  # Write the 3D coords
  cat("@1 # ",nVertices,"xyz coordinates\n",file=fc)
  if(is.null(sep)){
    # Amira seems fussy about having nicely aligned columns
    # using format with trim = FALSE (the default actually) 
    # and after getting rid of names results in a nicely justified table
    Coords=as.matrix(x$d[,c("X","Y","Z")])
    rownames(Coords)<-colnames(Coords)<-NULL
    write.table(format(Coords,trim=FALSE,scientific=FALSE),
                quote=F,row.names=FALSE,col.names=FALSE,file=fc)
  } else {
    # sep was explicitly specified, so use that
    write.table(x$d[chosenVertices,c("X","Y","Z")],col.names=F,row.names=F,file=fc,sep=sep)
  }
  
  # Write number of neighbours
  cat("\n@2 #",nVertices,"numbers of neighbours \n",file=fc)
  numNeighbours=integer(nVertices) # filled with 0s
  numNeighbours[sort(unique(EdgeList$CurPoint))]=table(EdgeList$CurPoint)
  write.table(numNeighbours,col.names=F,row.names=F,file=fc)
  
  # Write the Radii
  cat("\n@3 #",nVertices,"radii\n",file=fc)
  # NB Divide width by 2
  write.table(x$d$W[chosenVertices]/2,col.names=F,row.names=F,file=fc)
  
  # Write the edgelist information
  cat("\n@4 #",nEdgeList,"bidirectional edges\n",file=fc)
  #NB -1 since Amira is 0 indexed
  write.table(EdgeList$Neighbour-1,col.names=F,row.names=F,file=fc)
  
  # Write the origin information NB -1 since 0 indexed
  cat("\n@5 #n 1\n",file=fc)
  cat(x$StartPoint-1,"\n",file=fc)
  
  # Write the vertexTypeCounter information
  cat("\n@6 #",nVertices,"\n",file=fc)
  cat(paste(rep(0,nVertices),"\n"),sep="",file=fc)
  
  if(WriteAllSubTrees) {
    cat("\n@7 # subtrees\n",file=fc)
    if(ScaleSubTreeNumsTo1) x$d$SubTree=x$d$SubTree/max(x$d$SubTree)
    write.table(x$d$SubTree,col.names=F,row.names=F,file=fc)
  }
  cat("\n",file=fc)
  close(fc)
}

# write out a neuron in the basic AmiraMesh format which is the native format
# of amira for linesets (as opposed to the specialised skeletonize AM3D)
# WriteAllSubTrees will write out all the stored subtrees in a neuron 
# which has multiple subtrees (which is often true of ill-formed 
# skeletonize neurons)
write.neuron.hxlineset<-function(x, file=NULL, WriteAllSubTrees=TRUE,
                                 ScaleSubTreeNumsTo1=TRUE, WriteRadius=TRUE){
  
  # Make a seglist containing main or all segments
  SegList=as.seglist(x, all=WriteAllSubTrees, flatten=TRUE)
  # only use WriteAllSubTrees when there actually are multiple subtrees
  WriteAllSubTrees=WriteAllSubTrees && isTRUE(x$nTrees>1)

  chosenVertices=sort(unique(unlist(SegList)))
  nVertices=length(chosenVertices)
  # the number of points required to define the line segments
  # including the terminating -1s (1 for each segment)
  nLinePoints=length(unlist(SegList))+length(SegList) 
  
  # Write the header
  cat("# AmiraMesh ASCII 1.0\n",file=file)
  fc=file(file,open="at") # ie append, text mode
  
  cat("# Created by nat::write.neuron.hxlineset\n\n",file=fc)
  cat("define Lines",nLinePoints,"\n",file=fc)
  cat("define Vertices", nVertices,"\n\n",file=fc)
  
  cat("Parameters {\n",file=fc)
  cat("    ContentType \"HxLineSet\"\n",file=fc)
  cat("}\n\n",file=fc)
  sectionNumbers=c(Coordinates=1,LineIdx=2)
  cat("Vertices { float[3] Coordinates } = @1\n",file=fc)

  if(WriteRadius){
    if(any(is.na(x$d$W))) {
      warning("Width has NAs. Omitting invalid width data from file:", file)
      WriteRadius=FALSE
    } else {
      cat("Vertices { float Data } = @2\n",file=fc)
      sectionNumbers=c(Coordinates=1,Data=2,LineIdx=3)
    }
  }
  cat("Lines { int LineIdx } = @",sectionNumbers['LineIdx'],"\n",sep="",file=fc)
  if(WriteAllSubTrees) {
    sectionNumbers=c(sectionNumbers,Data2=max(sectionNumbers)+1)
    cat("Vertices { float Data2 } =@",sectionNumbers['Data2'],"\n",sep="",file=fc)
  }
  cat("\n",file=fc)
  
  # Write the 3D coords
  cat("@1 # ",nVertices,"xyz coordinates\n",file=fc)
  #write(t(x$d[,c("X","Y","Z")]),ncolumns=3,file=fc)
  write.table(x$d[chosenVertices,c("X","Y","Z")],col.names=F,row.names=F,file=fc)
  
  
  # Write the Radii
  if(WriteRadius){
    cat("\n@",sectionNumbers['Data']," # ",nVertices," width values\n",sep="",file=fc)
    # NB Divide width by 2
    write.table(x$d$W[chosenVertices]/2,col.names=F,row.names=F,file=fc,na='NaN')
  }
  
  # Write the segment information
  cat("\n@",sectionNumbers['LineIdx']," #",nLinePoints," line segments\n",sep="",file=fc)
  # nb have to -1 from each point because amira is 0 indexed
  # AND add -1 to each segment as a terminator
  lapply(SegList,function(x) cat(x-1,"-1 \n",file=fc))
  if(WriteAllSubTrees) {
    cat("\n@",sectionNumbers['Data2']," # subtrees\n",sep="",file=fc)
    if(ScaleSubTreeNumsTo1) x$d$SubTree=x$d$SubTree/max(x$d$SubTree)
    write.table(x$d$SubTree,col.names=F,row.names=F,file=fc)
  }
  close(fc)
}
