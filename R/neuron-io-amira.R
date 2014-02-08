# read neurons in the format produced by Amira skeletonize plugin
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

is.hxskel<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f,is.hxskel))
  
  tocheck=if(is.null(bytes)) f else bytes
  if(!is.amiramesh(tocheck)) return(FALSE)
  
  h=read.amiramesh.header(f, Parse=FALSE)
  isTRUE(any(grepl("ContentType.*SkeletonGraph",h,useBytes=T)))
}

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
  ng=ngraph(el, vertexlabels=coords$PointNo)
  as.neuron(ng, vertexData=coords, InputFileName=file, ...)
}

is.hxlineset<-function(f, bytes=NULL){
  if(!is.null(bytes) && length(f)>1)
    stop("can only supply raw bytes to check for single file")
  if(length(f)>1) return(sapply(f,is.hxlineset))
  
  tocheck=if(is.null(bytes)) f else bytes
  if(!is.amiramesh(tocheck)) return(FALSE)
  
  h=read.amiramesh.header(f, Parse=FALSE)
  isTRUE(any(grepl("ContentType.*HxLineSet",h,useBytes=T)))
}
