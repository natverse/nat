FindPlaneFromPointAndNormal<-function(p,n){
  # p is point, n is normal vector
  if(is.null(dim(p))) p=matrix(p,ncol=3)
  if(is.null(dim(n))) n=matrix(n,ncol=3)
  k=apply(n*p,1,sum)
  return(cbind(n,-k))
}


IntersectionLineSegmentAndPlane<-function(LineSegs,Plane){
  # the line seg is defined by two points in a 2x3nN where cols are X,Y,Z
  # and rows are the different points and 3rd dim is different line segs
  # the plane is defined the coefficients A,B,C,D
  # for  Ax + By + Cz + D = 0
  # This function will return NA if the plane does not lie between 
  # the 2 points
  
  if(identical(dim(LineSegs),as.integer(c(2,3)))){
    # only one point in 2X3 
    u=(sum(Plane[1:3]*LineSegs[1,]) +Plane[4])/(sum(Plane[1:3]*(LineSegs[1,]-LineSegs[2,])))
    #cat("u =",u,"\n")
    if(u>1 || u<0) return(c(NA,NA,NA))
    return(LineSegs[1,]+u*(LineSegs[2,]-LineSegs[1,]))
  } else {
    # 2x3xN
    # output will have cols for different line segs, rows for each of
    # three points
    u=(apply(Plane[1:3]*LineSegs[1,,],2,sum) +Plane[4])/apply(Plane[1:3]*(LineSegs[1,,]-LineSegs[2,,]),2,sum)
    #rval=LineSegs[1,,]+u*(LineSegs[2,,]-LineSegs[1,,])
    # nb t so that rows are points and cols are x,y,z
    rval=t(LineSegs[1,,])+apply(LineSegs[2,,]-LineSegs[1,,],1,"*",u)
    #cat("u =",u,"\n")
    rval[u>1 | u<0,]=NA
    rval
  }  
}


IntersectNeuronWithPlane<-function(ANeuron,APlane,ClosestPoint){
  # consider every line in neuron (ie each point and its parent)
  # make a 3d matrix R,C,i Rows are point 1 and 2, cols are XYZ and i are
  # multiple lines

  if(nrow(ANeuron$d)<2) stop("Need at least 2 points to define a line in Neuron")
  d=ANeuron$d
  points=unique(unlist(ANeuron$SegList))
  
  PointMatrix=array(0,dim=c(2,3,length(points)-1))
  # set up the ends of the lines
  PointMatrix[2,,]=t(as.matrix(d[points[-1],c("X","Y","Z")]))
  # set up the starts of the lines
  PointMatrix[1,,]=t(as.matrix(d[d$Parent[points[-1]],c("X","Y","Z")]))
  rval=IntersectionLineSegmentAndPlane(PointMatrix,APlane)
  # return any non NA rows
  rval=rval[!is.na(rval[,1]),]
  if(is.matrix(rval) && nrow(rval)>1 && !missing(ClosestPoint)){
    # find the closest intersection point
    squaredist=colSums((t(rval)-ClosestPoint)^2)
    rval=rval[which.min(squaredist),]
  }
  rval
}
