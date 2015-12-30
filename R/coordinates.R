#' Find XYZ coords corresponding to 1D indices into a 3D image
#' 
#' @description If you have an image-like object and you want to turn it into a 
#'   matrix of 3D coords then you need \code{ind2coord}. For the reverse
#'   operation we offer \code{\link{as.im3d.matrix}} which allows you to turn a
#'   matrix of 3D coordinates into an \code{im3d} image object.
#' @param inds indices into an image array (either 1D, for which \code{dims} 
#'   must be present, or a logical array).
#' @param ... extra arguments passed to methods.
#' @seealso \code{\link{coord2ind}}, \code{\link{sub2ind}}, 
#'   \code{\link{xyzpos}}, \code{\link{as.im3d.matrix}}
#' @export
ind2coord<-function(inds, ...) UseMethod("ind2coord")


#' @param dims dimensions of 3D image array.
#' @param voxdims vector of 3 voxel dimensions (width, height, depth).
#' @param origin the origin.
#' @export
#' @rdname ind2coord
ind2coord.default<-function(inds, dims, voxdims, origin, ...){
  if(length(dims) != 3 )
    stop('coords2ind only handles 3D data')
  if(is.matrix(voxdims))
    voxdims=as.numeric(voxdims)
  if(length(voxdims)!=length(dims))
    stop('number of voxel dimensions must match dimensionality of data')
  
  if(is.array(inds)){
    if(is.logical(inds))
      pixcoords = which(inds, arr.ind=TRUE)
    else if(is.integer(inds) || is.raw(inds))
      pixcoords = which(inds>0, arr.ind=TRUE)
    else stop("cannot handle numeric arrays - make a logical")
  } else if(is.logical(inds)){
    # 1d logical
    pixcoords=arrayInd(which(inds),.dim=dims)
  } else {
    # numbers 
    pixcoords=arrayInd(inds,.dim=dims)
  }
  
  if(nrow(pixcoords)==0) return(NULL)
  
  # then convert from pixel coords to physical coords
  # transpose to allow multiplication, then back again to give 3 cols
  # note that we must subtract 1 from 1-indexed pixcoords
  rval = if(missing(origin))
    t(t(pixcoords-1)*voxdims)
  else
    t(t(pixcoords-1)*voxdims+origin)
  colnames(rval)=c("X","Y","Z")
  rval
}


#' @export
#' @rdname ind2coord
ind2coord.array<-function(inds, voxdims=NULL, origin=NULL, ...){
  dims=dim(inds)
  if(is.null(voxdims)){
      stop("no voxdims supplied and inds has no physical dimension attributes")
  } else if(inherits(voxdims,'im3d')){
    if(is.null(origin))
      origin=origin(voxdims)
    voxdims=voxdims(voxdims)
  }
  
  if(is.null(origin))
    origin=rep(0,length(dims))
  
  ind2coord.default(inds, dims=dims, voxdims=voxdims, origin=origin, ...)
}

#' @export
#' @rdname ind2coord
ind2coord.im3d<-function(inds, voxdims=NULL, origin=NULL, ...){
  if(is.null(voxdims)) voxdims=voxdims(inds)
  if(is.null(origin)) origin=origin(inds)
  NextMethod("ind2coord", voxdims=voxdims, origin=origin, ...)
}


#' Find 1D indices into a 3D image given spatial coordinates
#' 
#' @param coords spatial coordinates of image voxels.
#' @param ... extra arguments passed to methods.
#' @export
coord2ind <- function(coords, ...) UseMethod("coord2ind")


#' @param imdims array dimensions of 3D image.
#' @param voxdims vector of 3 voxels dimensions (width, height, depth).
#' @param origin the origin of the 3D image.
#' @param aperm permutation order for axes.
#' @param Clamp ???
#' @param CheckRanges whether to check if coordinates are out of range.
#' @seealso \code{\link{ind2coord}}, \code{\link{sub2ind}}, \code{\link{ijkpos}}
#' @export
#' @rdname coord2ind
coord2ind.default<-function(coords,imdims,voxdims=NULL,origin=NULL,aperm,Clamp=FALSE,CheckRanges=!Clamp, ...){
  if(inherits(imdims, 'im3d')) {
    voxdims <- voxdims(imdims)
    origin <- origin(imdims)
    imdims <- dim(imdims)
  }
  else if(is.array(imdims)){
    if(missing(voxdims))
      voxdims=as.numeric(voxdims(imdims))
    if(missing(origin))
      origin=boundingbox(imdims)[c(1,3,5)]
    imdims=dim(imdims)
  }
  
  if(length(imdims) != 3)
    stop('coord2ind only handles 3D data')
  
  if(!is.matrix(coords))
    coords=matrix(coords,byrow=TRUE,ncol=length(coords))
  if(!missing(origin))
    coords=t(t(coords)-origin)
  
  pixcoords=t(round(t(coords)/voxdims))+1
  
  # make sure no points are out of range
  if(Clamp){
    pixcoords[,1]=pmin(imdims[1],pmax(1,pixcoords[,1]))
    pixcoords[,2]=pmin(imdims[2],pmax(1,pixcoords[,2]))
    pixcoords[,3]=pmin(imdims[3],pmax(1,pixcoords[,3]))
  } else if(CheckRanges){
    ranges=apply(pixcoords,2,range)
    if(any(ranges[2,]>imdims) || any(ranges[1,]<1))
      stop("pixcoords out of range")
  }
  
  # convert to 1d indices
  if (!missing(aperm))
    imdims=imdims[aperm]
  sub2ind(imdims,pixcoords)
}


#' Find 1D index given n-dimensional indices
#' 
#' Emulates the MATLAB function \code{sub2ind}.
#' @param dims vector of dimensions of object to index into.
#' @param indices vector of n-dimensional indices.
#' @export
sub2ind<-function(dims,indices){  
  # convert vector containing 1 coordinate into matrix
  if(!is.matrix(indices))
    indices=matrix(indices,byrow=TRUE,ncol=length(indices))
  if(length(dims)!=ncol(indices)){
    stop("indices must have the same number of columns as dimensions in dims")
  }
  k=cumprod(c(1,dims[-length(dims)]))
  ndx=1
  for(i in 1:length(dims)){
    v=indices[,i]
    if(any(v<1) || any(v>dims[i]))
      stop("index out of range")
    ndx=ndx+(v-1)*k[i]
  }
  ndx
}
