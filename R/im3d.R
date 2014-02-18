#' Construct an im3d object representing 3D image data, densities etc
#' 
#' @param x The object to turn into an im3d
#' @param dims The dimensions of the image array - may be overridden when 
#'   constructing an im3d in which the data block has been omitted
#' @param voxdims The voxel dimensions
#' @param origin the location (or centre) of the first voxel
#' @param BoundingBox,bounds Physical extent of image See
#'   \code{\link{boundingbox}} details for the distinction
#' @return An array with additional class \code{im3d}
#' @export
im3d<-function(x=numeric(0), dims=dim(x), voxdims=NULL, origin=NULL,
               BoundingBox=NULL, bounds=NULL){
  if(!inherits(x,'im3d'))
    class(x)<-c("im3d",class(x))
  
  boundSpecs=!c(is.null(BoundingBox), is.null(bounds), is.null(voxdims))
  if(sum(boundSpecs)<1){
    return(x)
  } else if(sum(boundSpecs)>1)
    stop("only 1 of boundingBox, bounds or voxdims can be supplied")
  if(!is.null(BoundingBox)){
    BoundingBox=boundingbox(BoundingBox)
    voxdims=voxdims(BoundingBox,dims=dims)
    attr(x,'BoundingBox')=BoundingBox
  } else if(!is.null(bounds)) {
    #FIXME add bounds
  } else if(!is.null(voxdims)) {
    BoundingBox=rbind(c(0,0,0),(dims-1)*voxdims)
    if(!is.null(origin)){
      BoundingBox=t(origin+t(BoundingBox))
      # we can only formally make a bounding box if we have an origin
      attr(x,'BoundingBox')=BoundingBox
    }
  }
  attr(x,"x")<-seq(BoundingBox[1],BoundingBox[2],len=dims[1])
  attr(x,"y")<-seq(BoundingBox[3],BoundingBox[4],len=dims[2])
  attr(x,"z")<-seq(BoundingBox[5],BoundingBox[6],len=dims[3])
  x
}

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
  
  latticeDims=attr(d,'dataDef')$Dims[[1]]
  im3d(d, dims=latticeDims, BoundingBox=attr(d,'Parameters')$BoundingBox)
}

#' Return voxel dimensions of an object
#' 
#' @param x An \code{im3d} object with associated voxel dimensions or a 2 x 3 
#'   BoundingBox \code{matrix}.
#' @param ... Additional arguments for methods
#' @return A numeric vector of length 3, NA when NULL
#' @export
#' @seealso \code{\link{boundingbox}}
#' @family im3d
voxdims<-function(x, ...) UseMethod("voxdims")

#' @S3method voxdims im3d
voxdims.im3d<-function(x, ...){
  voxdims(boundingbox(x), dim(x), ...)
}

#' @S3method voxdims default
#' @method voxdims default
#' @param dims The number of voxels in each dimension when x is a BoundingBox 
#'   matrix.
#' @rdname voxdims
voxdims.default<-function(x, dims, ...){
  if(length(x)){
    vd=diff(boundingbox(x, dims, ...))/(dims-1)
    return(as.vector(vd))
  } else rep(NA_real_, 3)
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
#' @family im3d
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

#' @method boundingbox default
#' @S3method boundingbox default
#' @param input Whether \code{x} defines the boundingbox or bounds of the image 
#'   (see details).
#' @rdname boundingbox
boundingbox.default<-function(x, dims, input=c("boundingbox",'bounds'), ...){
  input=match.arg(tolower(input),c("boundingbox",'bounds'))
  if(!length(x)) return(NULL)
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

#' @S3method dim im3d
dim.im3d<-function(x){
  dimx=NextMethod(generic='dim')
  if(is.null(dimx)){
    xyz=c('x','y','z')
    dimsavail=xyz[sapply(xyz, function(d) !is.null(attr(x,d)))]
    dimx=sapply(dimsavail, function(d) length(attr(x,d)), USE.NAMES=F)
  }
  dimx
}
