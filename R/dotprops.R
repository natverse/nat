#' dotprops: Neurons as point clouds with tangent vectors (but no connectivity)
#' @param x Object to be tested/converted
#' @rdname dotprops
#' @export
is.dotprops<-function(x) inherits(x,"dotprops")

#' @rdname dotprops
#' @export
as.dotprops<-function(x, ...){
  if(is.null(x)) return (NULL)
  if(!is.dotprops(x)) class(x)=c("dotprops",class(x))
  if(is.null(colnames(x$points))) colnames(x$points) <-c("X","Y","Z") 
  x
}

#' Arithmetic for dotprops objects
#' @param x A dotprops object
#' @param y A scalar or 3-vector that will be applied to the dotprops object
#' @return A new dotprops object
#' @export
#' @method * dotprops
#' @rdname dotprops-arithmetic
`*.dotprops` <- function(x,y) {
  ly=length(y)
  if(!ly%in%c(1,3)) stop("expects a numeric vector of length 1 or 3")
  xyzmatrix(x)<-t(t(xyzmatrix(x))*y)
  x
}

#' @export
#' @method + dotprops
#' @rdname dotprops-arithmetic
`+.dotprops` <- function(x,y) {
  ly=length(y)
  if(!ly%in%c(1,3)) stop("expects a numeric vector of length 1 or 3")
  xyzmatrix(x)<-t(t(xyzmatrix(x))+y)
  x
}

#' @method - dotprops
#' @export
#' @rdname dotprops-arithmetic
`-.dotprops` <- function(x,y) x+(-y)

#' @method / dotprops
#' @export
#' @rdname dotprops-arithmetic
`/.dotprops` <- function(x,y) x*(1/y)


#' Scale and Centre dotprops coords
#' 
#' @details Note that if scale=TRUE, the neuron will be rescaled to unit sd in 
#'   each axis likewise if center=TRUE, the neuron will be centred around the 
#'   axis means
#' @details note that this does not touch the tangent vectors, which may no 
#'   longer be valid for anisotopic scales.
#' @param x A dotprops object
#' @param center 3-vector to subtract from x,y,z coords. Note that it is
#'   possible to scale individual axes
#' @param scale 3-vector used to divide x,y,z coords
#' @return neuron with scaled coordinates
#' @method scale dotprops
#' @export
#' @seealso \code{\link{scale.default}}
scale.dotprops<-function(x,center=TRUE,scale=TRUE){
  d=xyzmatrix(x)
  if(!is.logical(center) && any(is.na(center))){
    # NA signals that we don't want to touch an axis
    dd=d[,!is.na(center)]
    centers=center[!is.na(center)]
    dds=scale(dd,center=centers,scale=FALSE)
    ds=d
    ds[,!is.na(center)]=dds
  } else if(is.logical(center) && length(center)>1){
    # FALSE signals that we don't want to touch an axis
    dd=d[,center]
    dds=scale(dd,center=TRUE,scale=FALSE)
    ds=d
    ds[,center]=dds
  } else {
    ds=scale(d,scale=scale,center=center)
  }
  xyzmatrix(x)=ds
  x
}

#' @description \code{dotprops} makes dotprops representation from raw 3d points
#'   (extracting vertices from S3 objects that have them)
#' @details \code{k} will default to 20 nearest neighbours when unset (i.e. when
#'   it has default value of NA) unless \code{x} is a dotprops object.
#' @param ... Additional arguments passed to methods
#' @export
#' @rdname dotprops
dotprops<-function(x, ...) UseMethod('dotprops')

#' @method dotprops dotprops
#' @S3method dotprops dotprops
#' @export
#' @rdname dotprops
dotprops.dotprops<-function(x, k=attr(x,'k'), ...) {
  dotprops(xyzmatrix(x), k=k, ...)
}

#' @S3method dotprops list
dotprops.list<-function(x, ...) {
  # FIXME - change to an abstract base class for objects with 3d vertices
  # rather than the completely generic list
  dotprops(xyzmatrix(x), ...)
}

#' @S3method dotprops neuronlist
#' @method dotprops neuronlist
#' @rdname dotprops
dotprops.neuronlist<-function(x, ...) {
  nlapply(x, dotprops, ...)
}

#' @S3method dotprops neuron
#' @method dotprops neuron
#' @rdname dotprops
dotprops.neuron<-function(x, Labels=NULL, ...) {
  if(is.null(Labels) || isTRUE(Labels)) Labels=x$d$Label
  else if(is.logical(labels) && labels==FALSE) Labels=NULL
  dotprops(xyzmatrix(x), Labels=Labels, ...)
}

#' @method dotprops default
#' @export
#' @rdname dotprops
#' @param k Number of nearest neighbours to use for tangent vector calculation
#'   (set to k=20 when passed NULL)
#' @param Labels Vector of labels for each point or \code{NULL} to accept 
#'   class-specific default behaviour for different S3 classes, \code{TRUE} 
#'   always to use labels when incoming object has them and \code{FALSE} never 
#'   to use labels.
#' @param na.rm Whether to remove \code{NA} points (default FALSE)
#' @importFrom RANN nn2
#' @references The dotprops format is essentially identical to that developed in:
#' 
#' Masse N.Y., Cachero S., Ostrovsky A., and Jefferis G.S.X.E. (2012). 
#' A mutual information approach to automate identification of neuronal clusters 
#' in \emph{Drosophila} brain images. Frontiers in Neuroinformatics 6 (00021).
#' \href{http://dx.doi.org/10.3389/fninf.2012.00021}{doi: 10.3389/fninf.2012.00021}
dotprops.default<-function(x, k=NULL, Labels=NULL, na.rm=FALSE, ...){
  # store labels from SWC format data if this is a neuron
  x=xyzmatrix(x)
  if(is.null(k)) k=20
  if(na.rm){
    narows=rowSums(is.na(x))>0
    if(any(narows)) x=x[!narows,]
  }
  npoints=nrow(x)
  if(npoints<k) stop("Too few points to calculate properties")
  if(ncol(x)!=3) stop("points must be a N x 3 matrix")
  
  alpha=rep(0,npoints)
  vect=matrix(0,ncol=3,nrow=npoints)
  
  nns=nn2(x,x,k=k)
  # transpose points to 3xN because 
  # R arithemtic of matric / vector operates column-wise
  pointst=t(x)
  for(i in 1:npoints){
    indNN=nns$nn.idx[i,]
    
    pt=pointst[,indNN]
    cpt=pt-rowMeans(pt)
    
    inertia=matrix(0,ncol=3,nrow=3)
    diag(inertia)=rowSums(cpt^2)
    inertia[1,2]<-inertia[2,1]<-sum(cpt[1,]*cpt[2,])
    inertia[1,3]<-inertia[3,1]<-sum(cpt[1,]*cpt[3,])
    inertia[2,3]<-inertia[3,2]<-sum(cpt[2,]*cpt[3,])
    v1d1<-eigen(inertia,symmetric=TRUE)
    
    alpha[i]=(v1d1$values[1]-v1d1$values[2])/sum(v1d1$values)
    vect[i,]=v1d1$vectors[,1]
  }
  rlist=list(points=x,alpha=alpha,vect=vect)
  rlist$labels=Labels
  attr(rlist,'k')=k
  return(as.dotprops(rlist))
}

#' all.equal method tailored to dotprops objects
#' 
#' This method is require because the direction vectors are computed using an 
#' eigen vector decomposition where the sign of the eigenvector is essentially 
#' random and subject to small numerical instabilities. Therefore it does not 
#' usually make sense to check the value of vect exactly.
#' @method all.equal dotprops
#' @param target,current dotprops objects to compare
#' @param check.attributes Whether to check attributes (false by default)
#' @param absoluteVectors Whether to check only the absolute value of eigen
#'   vectors for equality (default TRUE, see description)
#' @param ... Additional arguments passed to base \code{all.equal}.
#' @export
#' @examples
#' # equal using default 
#' kc1=kcs20[[1]]
#' kc1.recalc=dotprops(kc1)
#' # not equal due to differences in attributes and vectors
#' all.equal.default(kc1.recalc, kc1)
#' # still not equal because of tangent vector flipping
#' all.equal.default(kc1.recalc, kc1, check.attributes=FALSE)
#' # equal using appropriate method
#' stopifnot(isTRUE(all.equal(kc1.recalc, kc1)))
#' # NB identical when recalculated on same setup from same data
#' stopifnot(isTRUE(all.equal.default(kc1.recalc, dotprops(kc1))))
all.equal.dotprops<-function(target, current, check.attributes=FALSE, 
                             absoluteVectors=TRUE, ...){
  if(!is.dotprops(current)) stop("current is not a dotprop object")
  if(absoluteVectors){
    target$vect=abs(target$vect)
    current$vect=abs(current$vect)
  }
  NextMethod(check.attributes=check.attributes, ...)
}

#' 3D plots of dotprops objects using rgl package
#' 
#' @details Tangent vectors are plotted by \code{segments3d} and centered on the
#'   relevant point. Points are plotted by \code{points3d}.
#'   
#' @param x A dotprops object
#' @param scalevecs Factor by which to scale unit vectors (numeric, default: 
#'   1.0)
#' @param alpharange Restrict plotting to points with \code{alpha} values in 
#'   this range to plot (default: null => all points). See 
#'   \code{\link{dotprops}} for definition of \code{alpha}.
#' @param PlotPoints,PlotVectors Whether to plot points and/or tangent vectors 
#'   (logical, default: tangent vectors only)
#' @param UseAlpha Whether to scale tangent vector length by the value of 
#'   \code{alpha}
#' @param ... Additional arguments passed to \code{points3d} and/or 
#'   \code{segments3d}
#' @return invisible list of results of rgl plotting commands
#' @method plot3d dotprops
#' @export
#' @importFrom rgl plot3d
#' @seealso \code{\link{dotprops}, \link[rgl]{plot3d}, \link[rgl]{points3d}, 
#'   \link[rgl]{segments3d}}
#' @examples
#' open3d()
#' plot3d(kcs20[[1]])
#' clear3d()
#' plot3d(kcs20[[1]],col='red')
#' clear3d()
#' plot3d(kcs20[[1]],col='red',lwd=2)
#' plot3d(kcs20[[2]],col='green',lwd=2)
plot3d.dotprops<-function(x, scalevecs=1.0, alpharange=NULL,
                          PlotPoints=FALSE, PlotVectors=TRUE, UseAlpha=FALSE,...){
  # rgl's generic plot3d will dispatch on this
  if (!is.null(alpharange))
    x=subset(x,x$alpha<=alpharange[2] & x$alpha>=alpharange[1])
  rlist=list()
  if(PlotPoints)
    rlist$points=points3d(x$points,...)
  if(PlotVectors){
    halfvect=x$vect/2*scalevecs
    if(UseAlpha) halfvect=halfvect*x$alpha
    starts=x$points-halfvect
    stops=x$points+halfvect
    interleaved=matrix(t(cbind(starts,stops)),ncol=3,byrow=T)
    rlist$segments=segments3d(interleaved,...)
  }
  invisible(rlist)
}

#' Subset points in dotprops object that match given conditions
#' 
#' @details \code{subset} defines either logical or numeric indices, in which
#'   case these are simply applied to the matrices that define the points, vect
#'   etc OR a function (which is called with the 3d points array and returns T/F
#'   vector).
#' @param x A dotprops object
#' @param subset A subset of points defined by indices or a function (see Details)
#' @param ... Additional parameters (currently ignored)
#' @return subsetted dotprops object
#' @method subset dotprops
#' @export
#' @seealso \code{nat.as::prune.dotprops}
#' @examples
#' \dontrun{
#' s3d=select3d()
#' dp1=subset(dp,s3d(points))
#' # special case of previous version
#' dp2=subset(dp,s3d)
#' stopifnot(all.equal(dp1,dp2))
#' dp2=subset(dp,alpha>0.5 & s3d(pointd))
#' dp3=subset(dp,1:10)
#' }
subset.dotprops<-function(x, subset, ...){
  e <- substitute(subset)
  r <- eval(e, x, parent.frame())
  if (!is.logical(r) && !is.numeric(r)) {
    # a function that tells us whether a point is in or out
    if(is.function(r)) r=subset(x$points)
    else stop("Cannot evaluate subset")
  }
  if(is.logical(r)) r <- r & !is.na(r)
  else if(!is.numeric(r)) stop("Subset must evaluate to a logical or numeric index")
  
  x$points=x$points[r,,drop=F]
  x$alpha=x$alpha[r]
  x$vect=x$vect[r,,drop=F]
  if(!is.null(x$labels)) x$labels=x$labels[r]
  x
}
