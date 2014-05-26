#' Calculate number of potential synapses between two neurons
#' 
#' This implements the method of Stepanyants and Chklovskii
#' 
#' @param a,b neurons or neuronlists
#' @param s the approach distance to consider a potential synapse
#' @param sigma the smoothing parameter in the approximate method (see details)
#' @param method Whether to use the direct or approximate method (see details)
#' @param bounds Optional bounding box to restrict comparison
#' @param ... Additional arguments passed to methods
#' @export
#' @references
#' Neurogeometry and potential synaptic connectivity.
#' Stepanyants A, Chklovskii DB.
#' Trends Neurosci. 2005 Jul;28(7):387-94.
#' \url{http://dx.doi.org/10.1016/j.tins.2005.05.006}
#' @examples
#' potential_synapses(Cell07PNs[1], Cell07PNs[1:3], s=2)
potential_synapses<-function(a, b, s, ...) UseMethod('potential_synapses')

#' @method potential_synapses neuronlist
#' @export
#' @rdname potential_synapses
potential_synapses.neuronlist<-function(a, b, s, ...) {
  sapply(a, potential_synapses, b, s, ...)
}

#' @method potential_synapses neuron
#' @export
#' @rdname potential_synapses
potential_synapses.neuron<-function(a, b, s, sigma=s, bounds, method=c("direct", "approx"), ...) {
  method=match.arg(method, c("direct","approx"))
  
  if(is.neuronlist(b)) 
    return(sapply(b, function(x) potential_synapses(a, x, s, method=method, ...)))
  
  a.sel=MakeStartEndList(a)
  b.sel=MakeStartEndList(b)
  if(!missing(bounds)){
    a.sel=restrictToBounds(a.sel,bounds)
    b.sel=restrictToBounds(b.sel,bounds)
  }
  if(nrow(a.sel)==0 || nrow(b.sel)==0) return(0)
  if(method=="direct"){
    DirectPotentialSynapses(a.sel,b.sel,s,...)
  } else if (method=="approx") {
    PotentialSynapses(a.sel,b.sel,s, sigma=sigma, ...)
  }
}

#' @method potential_synapses dotprops
#' @param seglength how long to consider each distance between points.
#' @export
#' @rdname potential_synapses
potential_synapses.dotprops<-function(a, b, s, sigma=s, seglength=1, bounds=NULL, method=c("direct", "approx"), ...) {
  method=match.arg(method, c("direct","approx"))
  if(is.neuronlist(b))
    return(sapply(b, function(x) potential_synapses(a, x, s=s, sigma=sigma, seglength=seglength, bounds=bounds, method=method, ...)))

  if(!is.null(bounds)){
    a.new <- a
    b.new <- b
    a.new$points <- restrictToBounds(a$points, bounds)
    b.new$points <- restrictToBounds(b$points, bounds)
    a.new$vect <- a.new$vect[apply(a$points, 1, function(x) all(x %in% a.new$points)), ]
    b.new$vect <- b.new$vect[apply(b$points, 1, function(x) all(x %in% b.new$points)), ]
    a <- a.new
    b <- b.new
  }

  if(nrow(a$points)==0 || nrow(b$points)==0) return(0)
  if(method=="direct"){
    a.sel <- cbind(a$points[-nrow(a$points), ], a$points[-1, ])
    b.sel <- cbind(b$points[-nrow(b$points), ], b$points[-1, ])
    DirectPotentialSynapses(a.sel,b.sel,s,...)
  } else if (method=="approx") {
    PotentialSynapses(a, b, s=s, sigma=sigma, seglength=seglength)
  }
}

PotentialSynapses <- function(a, b, s, sigma, ...) UseMethod("PotentialSynapses")

#' @method PotentialSynapses default
PotentialSynapses.default <- function(a, b, s=2, sigma=2) {
  #Compare for matrices of input data rather than neurons
  
  # short circuit if there are no points to check in one list or other!
  if(nrow(a)*nrow(b)==0) return (0)
  
  # calculate midpoint position vectors
  ra=(a[,4:6]+a[,1:3])/2
  rb=(b[,4:6]+b[,1:3])/2
  
  # calculate segment  vectors
  na=a[,4:6]-a[,1:3]
  nb=b[,4:6]-b[,1:3]
  
  # calc lengths
  la=sqrt(rowSums(na*na))
  lb=sqrt(rowSums(nb*nb))	
  # deal with any zero length segments
  na=na[la>0,];	nb=nb[lb>0,]
  ra=ra[la>0,];	rb=rb[lb>0,]
  la=la[la>0];  lb=lb[lb>0]
  
  lab=outer(la,lb) # ie has na rows and nb cols
  
  # try replacing this apply by an explicit for loop
  # turns out to be about 30% quicker!
  #sintheta=sin(apply(nb,1,thetaBetween,na))
  # has na rows by nb cols
  sintheta=lab # so just use this to init
  h=nrow(na);w=ncol(nb)
  for(j in 1:h){
    sintheta[j,]=thetaBetween(nb,na[j,])
  }
  
  raminusrb=rowbyrow(ra,rb,"-")
  # find the squared magnitude of the difference vector ra-rb
  l2rab=rowSums(raminusrb*raminusrb)
  #l2rab.old=l2rab
  # reorder the vector
  dim(l2rab)=dim(lab)
  FourSigma2=4*sigma^2
  denom=(4*pi*sigma^2)^(3/2)
  expterm=exp(-l2rab/FourSigma2)/(denom)
  
  rval=2 * s * sum(lab * sintheta * expterm)
  
  #return(list(ra,rb,na,nb,la,lb,lab,sintheta,raminusrb,l2rab.old,l2rab,expterm,rval))
  return(rval)
}

#' @method PotentialSynapses dotprops
PotentialSynapses.dotprops <- function(a, b, s, sigma, seglength) {
  # short circuit if there are no points to check in one list or other!
  if(nrow(a$points) * nrow(b$points) == 0) return(0)

  la <- rep(seglength, nrow(a$points))
  lb <- rep(seglength, nrow(b$points))

  lab <- outer(la, lb)

  sintheta <- lab
  h <- nrow(a$points); w <- nrow(b$points)
  for(j in 1:h) {
    sintheta[j, ] <- thetaBetween(b$vect, a$vect[j, ])
  }

  raminusrb <- rowbyrow(a$points,b$points,"-")
  l2rab <- rowSums(raminusrb * raminusrb)
  dim(l2rab) <- dim(lab)
  FourSigma2 <- 4 * sigma^2
  denom <- (4 * pi * sigma^2)^(3 / 2)
  expterm <- exp(-l2rab / FourSigma2) / (denom)
  rval <- 2 * s * sum(lab * sintheta * expterm)
  return(rval)
}

#@+node:jefferis.20060305214152.4:DirectPotentialSynapses
DirectPotentialSynapses<-function(a,b,s=2,maxlineseglength=0.5,returnDistanceList=FALSE){
  # This takes a pair of matrices representing neuron segments
  # and _directly_calculates the number of potential synapses
  # ie regions of approach less than s
  
  #NB It is essential that a and b are formatted as matrices not vectors
  if (!is.matrix(a) || !is.matrix(b)) stop("a and b must be matrices")
  
  # 2) for each of the segs in a	
  # find the segs in b less than s+maxlineseglength away
  # since we only check the head of each segment with
  # the head of the ref segment we should look to see if they are
  # less than double the max seg length + max accecptable distance
  
  # o-----x      x-----o
  # eg in this arrangement we are measuring from o to o
  
  segswithin<-function(seg,segs,d){
    upperRange=seg[1:3]+d
    lowerRange=seg[1:3]-d
    
    #belowUpperRange <- segs[,1:3] <= upperRange
    #aboveLowerRange <- lowerRange >= segs[,1:3]
    
    # if(nrow(segs)==1) allWithinRange = all(belowUpperRange & aboveLowerRange)
    # else allWithinRange=apply(cbind(belowUpperRange , aboveLowerRange), 1, all)
    
    #return(segs[allWithinRange, ])
    
    return(segs[segs[,1]<=upperRange[1] & segs[,2]<=upperRange[2]
                & segs[,3]<=upperRange[3] & segs[,1]>=lowerRange[1]
                & segs[,2]>=lowerRange[2] & segs[,3]>=lowerRange[3],] )
  }
  # 	selectedBSegs=apply(a,1,segswithin,b,maxlineseglength*2+s*2)
  selectedBSegs=lapply(seq.int(nrow(a)),function(i) segswithin(a[i,],b,maxlineseglength*2+s*2))
  # This is redundant of course, but ...
  # selectedBSegs=apply(b,1,segswithin,a,maxlineseglength*2+s*2)
  if(length(selectedBSegs)==0)
    return (if(returnDistanceList) list() else 0)
  # This part could fairly easily be rewritten with a sparse matrix
  # rather than a list (which should be 
  distances=list()
  if(is.null(rownames(a))) rownames(a)=seq.int(nrow(a))
  for(i in 1:nrow(a)){
    #cat("i =",i,"  ")
    if(is.matrix(selectedBSegs[[i]]) && nrow(selectedBSegs[[i]])>0){
      distances[[ rownames(a)[i] ]]=
        apply(selectedBSegs[[i]],1,dist3D_Segment_to_Segment,a[i,])
      
    } else if(length(selectedBSegs[[i]])>0){
      # handle case of one segment only	
      distances[[ rownames(a)[i] ]]=
        dist3D_Segment_to_Segment(selectedBSegs[[i]],a[i,])
      # apply handles setting names for vectors
      # but this needs to be done manually when there is only
      # one segment
      names(distances[[ rownames(a)[i] ]])=names(selectedBSegs[i])
    }
  }
  if(returnDistanceList) return(distances)
  else return(sum(unlist(distances)<=s))
}

MakeStartEndList<-function(ANeuron,mask=seq(len=length(ANeuron$SegList))){
  # make an ordered list of the start/end points for each seg in form
  # Start x y z , end x y z   ie 6 col matrix
  
  StartIdxs=unlist(sapply(ANeuron$SegList[mask],function(x) x[-length(x)]))
  EndIdxs=unlist(sapply(ANeuron$SegList[mask],function(x) x[-1]))
  
  d=data.matrix(ANeuron$d[,c("X","Y","Z")])
  cbind(d[StartIdxs,],d[EndIdxs,])		
}

dotprod=function(a,b){
  # expects 2 matrices with n cols each
  c=a*b
  if(length(dim(c))>1)   rowSums(c)
  else sum(c)
}

# note use of zapsmall in case of rounding error
thetaBetween=function(a,b) {
  if(is.vector(a) && is.matrix(b)){
    a=matrix(a,nrow=nrow(b),ncol=length(a),byrow=T)
  } else if(is.vector(b) && is.matrix(a)){
    b=matrix(b,nrow=nrow(a),ncol=length(b),byrow=T)
  }
  acos(zapsmall(dotprod(a,b)/(normbyrow(a)*normbyrow(b))))
}

normbyrow=function(a){
  # returns euclidean norm (by row if reqd)
  c=a*a
  if(length(dim(c))>1) 	sqrt(rowSums(c))
  else sqrt(sum(c))
}

rowbyrow<-function(X,Y,FUN="-",...){
  if(ncol(X)!=ncol(Y)) return(NA)
  FUN=match.fun(FUN)
  rX=nrow(X)
  rY=nrow(Y)
  X=matrix(t(X),nrow=rX*rY,ncol=ncol(X),byrow=T)
  Y <- matrix(rep(Y, rep.int(rX, length(Y))),ncol=ncol(X))
  FUN(X,Y,...)
}

# restrict points to a bounding box
# @param a Nx3 matrix of points
# @param bounds bounding box vector (x0,x1,y0,y1,z0,z1)
restrictToBounds<-function(a, bounds){
  a[  a[,1]>=bounds[1] & a[,1]<=bounds[2] &
        a[,2]>=bounds[3] & a[,2]<=bounds[4] &
        a[,3]>=bounds[5] & a[,3]<=bounds[6], ]
}
