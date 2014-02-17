#' Read/Write calibrated 3D blocks of image data
#' 
#' @details Currently only nrrd and amira formats are implemented. Furthermore 
#'   implementing a registry to allow extension to arbitrary formats remains a 
#'   TODO item.
#' @param file Character vector describing a single file
#' @param ReadData Whether to read the data itself or return metadata only.
#'   Default: TRUE.
#' @param ... Arguments passed to methods
#' @return For \code{read.im3d} an objecting inheriting from base \code{array} 
#'   and \code{im3d} classes.
#' @export
#' @name im3d-io
#' @aliases read.im3d
#' @seealso \code{\link{read.nrrd}, \link{read.amiramesh}}
read.im3d<-function(file, ReadData=TRUE, ...){
  ext=sub(".*(\\.[^.])","\\1",file)
  x=if(ext%in%c('.nrrd','.nhdr')){
    read.nrrd(file, ReadData=ReadData, ...)
  } else if(ext%in%c(".am",'.amiramesh')){
    if(ReadData) read.im3d.amiramesh(file, ...)
    else read.im3d.amiramesh(file, sections=NA, ...)
  } else {
    stop("Unable to read data saved in format: ",ext)
  }
  if(!inherits(x,'im3d'))
    class(x)<-c("im3d",class(x))
  x
}

#' @param x The image data to write (an im3d, or capable of being interpreted as
#'   such)
#' @rdname im3d-io
#' @seealso \code{\link{write.nrrd}}
#' @export
write.im3d<-function(x, file, ...){
  ext=sub(".*(\\.[^.])","\\1",file)
  if(ext%in%c('.nrrd','.nhdr')){
    write.nrrd(x, file, ...)
  } else if(ext%in%c(".am",'.amiramesh')){
    write.amiramesh(x, file, ...)
  } else {
    stop("Unable to write data in format: ",ext)
  }
}

read.im3d.amiramesh<-function(file, ...){
  d<-read.amiramesh(file, ...)
  latticeDims=dim(d)
  latticeBounds=attr(d,'Parameters')$BoundingBox
  if(length(latticeBounds)>0){
    attr(d,"BoundingBox")<-latticeBounds
    attr(d,"x")<-seq(latticeBounds[1],latticeBounds[2],len=latticeDims[1])
    attr(d,"y")<-seq(latticeBounds[3],latticeBounds[4],len=latticeDims[2])
    attr(d,"z")<-seq(latticeBounds[5],latticeBounds[6],len=latticeDims[3])
  }
  d
}

#' Return voxel dimensions of an object
#' 
#' @param d An image like object with associated voxel dimensions
#' @param ... Additional arguments for methods
#' @export
voxdims<-function(d, ...) UseMethod("voxdims")

#' @S3method voxdims default
voxdims.default<-function(d, ...){
  if(all(c("x","y","z") %in% names(attributes(d)))){
    originaldims=sapply(attributes(d)[c("x","y","z")],length)
  } else {
    originaldims=dim(d)
  }
  if (!is.null(attr(d,"bounds")))
    # bounds = outer limit of voxels
    return(diff(matrix(attr(d,"bounds"),nrow=2))/originaldims)
  else if (!  is.null(attr(d,"BoundingBox"))) {
    # BoundingBox = CENTRES of outer voxels (like Amira)
    # therefore includes 1 fewer voxel in each dimension
    return(diff(matrix(attr(d,"BoundingBox"),nrow=2))/(originaldims-1))
  } 
  #warning("Cannot find bounds or BoundingBox attribute")
  return(NULL)
}

#' Get the bounding box of an im3d volume or other compatible object
#' 
#' @details The bounding box is defined as the cuboid with vertices at the most 
#'   extreme vertices of an image, \emph{when those vertices are assumed to have
#'   a single position (sometimes thought of as their centre) }\strong{and no 
#'   physical extent.}
#' @param x A vector, matrix or im3d object or, for \code{boundingbox.character}
#'   a character vector specifying a file.
#' @param ... Additional arguments passed to methods
#' @export
boundingbox<-function(x, ...) UseMethod("boundingbox")

#' @method boundingbox im3d
#' @S3method boundingbox im3d
#' @param dims The dimensions of the image array - can be used to override 
#'   dim(x) e.g. when only the metadata for an im3d has been read in.
#' @export
#' @rdname boundingbox
boundingbox.im3d<-function(x, dims=dim(x), ...) {
  if(!is.null(attr(x,"BoundingBox"))) attr(x,"BoundingBox")
  else if(!is.null(b<-attr(x,"bounds"))) {
    boundingbox(b, dims, ...)
  } else {
    bb=sapply(c('x','y','z'),
                  function(d) {ll=attr(x,d);c(ll[1],ll[length(ll)])}, USE.NAMES=F)
    if(is.null(dims))
      dims=sapply(c('x','y','z'),function(d) length(attr(b,d)),USE.NAMES=F)
    boundingbox(bb, dims)
  }
}

#' @S3method boundingbox character
boundingbox.character<-function(x, ...) {
  if(!file.exists(x))
    stop("Unable to find a file at path: ",x)
  
  boundingbox(read.im3d(x, ReadData=FALSE))
}

#' @S3method boundingbox default
boundingbox.default<-function(x, dims, input=c("boundingbox",'bounds'), ...){
  input=match.arg(tolower(input),c("boundingbox",'bounds'))
  if(!length(x)) NULL
  if(is.vector(x)) {
    if(length(x)!=6) stop("Must supply a vector of length 6")
    x=matrix(x,nrow=2)
  } else if(is.matrix(x)){
    if(!isTRUE(all.equal(dim(x),c(2L,3L),check.attributes=FALSE)))
      stop("Must supply a 2 x 3 matrix of physical extents")
  }
  if(input=='bounds'){
    if(missing(dims)) stop("must supply dimensions when input is of type bounds!")
    # we need to find the voxel dimensions in order to subtract off a
    # half voxel dim in each axis
    halfVoxelDims=voxdims(x, dims=dims)/2
    x[1,]=x[1,]+halfVoxelDims
    x[2,]=x[2,]-halfVoxelDims
  }
  # zap small gets rid of FP rounding errors
  zapsmall(x)
}
