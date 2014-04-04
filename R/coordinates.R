#' Find 1D indices into a 3D image given spatial coordinates
#' 
#' @param coords spatial coordinates of image voxels.
#' @export
coord2ind <- function(coords, ...) UseMethod("coord2ind")


#' @param imdims array dimensions of 3D image.
#' @param voxdims vector of 3 voxels dimensions (width, height, depth).
#' @param aperm permutation order for axes.
#' @param Clamp ???
#' @param CheckRanges whether to check if coordinates are out of range.
#' @seealso \code{\link{ind2coord}}, \code{\link{ind2sub}}
#' @S3method coord2ind default
#' @rdname coord2ind
coord2ind.default<-function(coords,imdims,voxdims=NULL,origin=NULL,aperm,Clamp=FALSE,CheckRanges=!Clamp){
  if(inherits(imdims, 'im3d')) {
    voxdims <- voxdims(imdims)
    origin <- origin(imdims)
    imdims <- dim(imdims)
  }
  else if(is.array(imdims)){
    if(missing(voxdims))
      voxdims=as.numeric(voxdim(imdims))
    if(missing(origin))
      origin=getBoundingBox(imdims)[c(1,3,5)]
    imdims=dim(imdims)
  }
  
  if(length(imdims) != 3)
    stop('coord2ind only handles 3d data')
  
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
