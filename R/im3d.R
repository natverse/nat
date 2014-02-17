#' Read a 3D block of image data
#' 
#' @details Currently only nrrd and amira formats are implemented. Furthermore 
#'   implementing a registry to allow extension to arbitrary formats remains a
#'   TODO item.
#' @param file Character vector describing a single file to read
#' @param ... Arguments passed to methods
#' @export
#' @seealso \code{\link{read.nrrd}, \link{read.amiramesh}}
read.im3d<-function(file, ...){
  ext=sub(".*(\\.[^.])","\\1",file)
  x=if(ext%in%c('.nrrd','.nhdr')){
    read.nrrd(file, ...)
  } else if(ext%in%c(".am",'.amiramesh')){
    read.im3d.amiramesh(file, ...)
  } else {
    stop("Unable to read data saved in format: ",ext)
  }
  if(!inherits(x,'im3d'))
    class(x)<-c("im3d",class(x))
  x
}

#' @param x The image data to write (an im3d, or capable of being interpreted as
#'   such)
#' @seealso \code{\link{write.nrrd}}
#' @export
write.im3d<-function(x, file, ...){
  ext=sub(".*(\\.[^.])","\\1",file)
  if(ext%in%c('.nrrd','.nhdr')){
    write.nrrd(x, file, ...)
  } else if(ext%in%c(".am",'.amiramesh')){
    message("write.im3d not yet implemented for amirmamesh format")
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
