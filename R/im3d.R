#' Construct an im3d object representing 3D image data, densities etc
#' 
#' \code{im3d} objects consist of a data array with attributes defining the 
#' spatial positions at which the voxels are located. There should always be a 
#' \code{BoundingBox} attribute which defines the physical extent of the volume 
#' in the same manner as the Amira 3d visualisation and analysis software. This 
#' corresponds to the \strong{node} centers option in the
#' \href{http://teem.sourceforge.net/nrrd/format.html}{NRRD format}.
#' @param x The object to turn into an im3d
#' @param dims The dimensions of the image array - may be overridden when 
#'   constructing an im3d in which the data block has been omitted
#' @param voxdims The voxel dimensions
#' @param origin the location (or centre) of the first voxel
#' @param BoundingBox,bounds Physical extent of image. See the details section 
#'   of \code{\link{boundingbox}}'s help for the distinction.
#' @return An array with additional class \code{im3d}
#' @details We follow Amira's convention of setting the bounding box equal to 
#'   voxel dimension (rather than 0) for any dimension with only 1 voxel.
#' @export
#' @family im3d
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
    voxdims=voxdims(BoundingBox,dims=dims)
    attr(x,'BoundingBox')=BoundingBox
  } else if(!is.null(bounds)) {
    #FIXME add bounds
  } else if(!is.null(voxdims)) {
    corrected_dims=pmax(dims-1, 1)
    BoundingBox=rbind(c(0,0,0),corrected_dims*voxdims)
    if(!is.null(origin)){
      BoundingBox=t(origin+t(BoundingBox))
    }
  }
  # always add a bounding box
  attr(x,'BoundingBox')=boundingbox(BoundingBox)
  attr(x,'origin')=origin
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
#'   Default: TRUE
#' @param SimplifyAttributes When \code{TRUE} leave only core im3d attributes.
#' @param ReadByteAsRaw Whether to read byte values as R \code{\link{raw}}
#'   arrays. These occupy 1/4 memory but arithmetic is less convenient.
#'   (default: FALSE)
#' @param ... Arguments passed to methods
#' @return For \code{read.im3d} an objecting inheriting from base \code{array} 
#'   and \code{im3d} classes.
#' @export
#' @name im3d-io
#' @aliases read.im3d
#' @family im3d
#' @seealso \code{\link{read.nrrd}, \link{read.amiramesh}}
read.im3d<-function(file, ReadData=TRUE, SimplifyAttributes=FALSE,
                    ReadByteAsRaw=FALSE, ...){
  ext=sub(".*(\\.[^.])","\\1",file)
  x=if(ext%in%c('.nrrd','.nhdr')){
    read.nrrd(file, ReadData=ReadData, ReadByteAsRaw=ReadByteAsRaw, ...)
  } else if(ext%in%c(".am",'.amiramesh')){
    if(ReadData) read.im3d.amiramesh(file, ReadByteAsRaw=ReadByteAsRaw, ...)
    else read.im3d.amiramesh(file, sections=NA, ReadByteAsRaw=ReadByteAsRaw, ...)
  } else {
    stop("Unable to read data saved in format: ",ext)
  }
  if(SimplifyAttributes){
    coreattrs=c("BoundingBox",'origin','x','y','z')
    mostattributes(x)<-attributes(x)[coreattrs]
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
  
  # Amira does not store the "space origin" separately as is the case for nrrds
  # Have decided that we should always store the origin inferred from the
  # BoundingBox
  bb=attr(d,'Parameters')$BoundingBox
  origin <- if(length(bb)) bb[c(1,3,5)] else NULL
  im3d(d, dims=attr(d,'dataDef')$Dims[[1]], BoundingBox=bb, origin=origin)
}

#' Return voxel dimensions of an object
#' 
#' @description This would properly be thought of as the voxel spacing when 
#'   voxels are assumed not to have a physical extent (only a location).
#' @param x An \code{im3d} object with associated voxel dimensions or a 2 x 3 
#'   BoundingBox \code{matrix}.
#' @param ... Additional arguments for methods
#' @return A numeric vector of length 3, NA when missing.
#' @details We follow Amira's convention of returning a voxel dimension equal to
#'   the bounding box size (rather than 0) for any dimension with only 1 voxel.
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
#' @family im3d
voxdims.default<-function(x, dims, ...){
  if(length(x)){
    corrected_dims=pmax(dims-1,1)
    vd=diff(boundingbox(x, dims, ...))/(corrected_dims)
    return(as.vector(vd))
  } else rep(NA_real_, 3)
}

#' Get the bounding box of an im3d volume or other compatible object
#' 
#' @details The bounding box is defined as the position of the voxels at the two
#'   opposite corners of the cuboid encompassing an image, \emph{when each voxel
#'   is assumed to have a single position (sometimes thought of as its centre) 
#'   \strong{and no physical extent.}} When written as a vector it should look 
#'   like: \code{c(x0,x1,y0,y1,z0,z1)}. When written as a matrix it should look 
#'   like: \code{rbind(c(x0,y0,z0),c(x1,y1,z1))} where x0,y0,z0 is the position
#'   of the origin.
#' @param x A vector or matrix specifying a bounding box, an \code{im3d} object 
#'   or, for \code{boundingbox.character}, a character vector specifying a file.
#' @inheritParams voxdims
#' @export
#' @family im3d
#' @examples
#' boundingbox(c(x0=0,x1=10,y0=0,y1=20,z0=0,z1=30))
boundingbox<-function(x, ...) UseMethod("boundingbox")

#' @method boundingbox im3d
#' @S3method boundingbox im3d
#' @export
#' @rdname boundingbox
boundingbox.im3d<-function(x, dims=dim(x), ...) {
  if(!is.null(attr(x,"BoundingBox"))) attr(x,"BoundingBox")
  else if(!is.null(b<-attr(x,"bounds"))) {
    boundingbox(b, dims, ...)
  } else {
    bb=sapply(c('x','y','z'),
                  function(d) {ll=attr(x,d);c(ll[1],ll[length(ll)])}, USE.NAMES=F)
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

image.gjdens<-function(x=NULL,y=NULL,
                       z, zlim=NULL, xlim=range(x,finite=TRUE), ylim=range(y,finite=TRUE),
                       plotdims=NULL,flipdims='y',filledContour=FALSE,asp=NA, axes=FALSE,
                       xlab=NULL,ylab=NULL,
                       nlevels=20,levels = pretty(zlim, nlevels+1),
                       color.palette=jet.colors,col = color.palette(length(levels) - 1),...){
  
  # # function which will extend R's image function
  # by 
  # 1. allowing images to have inverted x or ylims.
  #    - What would be the best here - I thought of just checking whether
  #    xlim or ylim were reversed, but I think that an actual flip command
  #    is preferable.
  # 2. Defaulting to a suitable colour ramp
  #@    << handle z as first argument >>
  #@+node:jefferis.20051016235253.1:<< handle z as first argument >>
  if (missing(z)) {
    if (!is.null(x)) {
      z <- x
      attributes(z)<-attributes(x)
      x <- NULL
    }
    else stop("no 'z' matrix specified")
  }
  #@-node:jefferis.20051016235253.1:<< handle z as first argument >>
  #@nl
  #@    << set x y z >>
  #@+node:jefferis.20051024111842:<< set x y z >>
  # don't try and plot anything if we have malformed zlims
  if(is.null(zlim)) zlim=range(z,finite=TRUE)
  if(!all(is.finite(zlim))){
    warning(paste("supplied zlim is not finite:",zlim))
    zlim=c(0,0)
  }
  
  if(!is.null(plotdims)){
    plotdims=tolower(plotdims); plotdims=unlist(strsplit(plotdims,split=""))
    if(is.null(x)) x=attr(z,plotdims[1])
    if(is.null(y)) y=attr(z,plotdims[2])
  } else if(!is.null(attr(z,"ProjDim"))){
    # if this is a projection, then choose correct axes to display
    plotdims=setdiff(c("x","y","z"),attr(z,"ProjDim"))
    if(is.null(x)) x=attr(z,plotdims[1])
    if(is.null(y)) y=attr(z,plotdims[2])
  } else if (all( c("x","y")%in%names(attributes(z)) )){
    if(is.null(x)) x=attr(z,"x")
    if(is.null(y)) y=attr(z,"y")
  }
  # If we still haven't set anything, then use default
  if(is.null(x)) x=seq(0,1,len=nrow(z))
  if(is.null(y)) y=seq(0,1,len=ncol(z))
  if(is.null(plotdims)) plotdims=c("x","y")
  #@-node:jefferis.20051024111842:<< set x y z >>
  #@nl
  #@    << handle flipdims >>
  #@+node:jefferis.20051016235253:<< handle flipdims >>
  numbers=1:3;names(numbers)=letters[24:26]
  
  # what about transposing matrix if axes have been swapped?
  if(numbers[plotdims[1]]>numbers[plotdims[2]]){
    z=t(z)
  }
  
  if(is.numeric(flipdims)) flipdims=names(numbers(flipdims))
  if(is.character(flipdims)) {
    flipdims=unlist(strsplit(tolower(flipdims),split=""))
    # nb at this stage we assume z looks like our axes, so
    # we don't try to match axis names etc
    if(plotdims[1]%in%flipdims) z<-flip.array(z,1)
    if(plotdims[2]%in%flipdims) z<-flip.array(z,2)
  }
  
  # now reverse axes if required
  if(plotdims[1]%in%flipdims) x=-rev(x)
  if(plotdims[2]%in%flipdims) y=-rev(y)
  #@nonl
  #@-node:jefferis.20051016235253:<< handle flipdims >>
  #@nl
  
  
  if(filledContour){
    plot(0,0,xlim,ylim,type='n',asp=asp,axes=FALSE,
         xlab=plotdims[1],ylab=plotdims[2],xaxs="i",yaxs="i",...)
    if (!is.double(z)) storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
                            col = col))
  } else {
    image(x=x,y=y,z=z,zlim=zlim,xlim=xlim,ylim=ylim,col=col,asp=asp,axes=FALSE,
          xlab=plotdims[1],ylab=plotdims[2],...)
  }
  if(axes){
    axis(2,pretty(par("usr")[3:4]),abs(pretty(par("usr")[3:4])))
    axis(1,pretty(par("usr")[1:2]),abs(pretty(par("usr")[1:2])))
  }
  # Return info that will be useful for creating scalebars
  invisible(list(zlim=zlim,nlevels.actual=length(levels),nlevels.orig=nlevels,
                 levels=levels,colors=col))
}

projection<-function(a,projdim='z',projfun=c('integrate','mean','sum'),warn=F,na.rm=T,mask=NULL,...){
  ndims=length(dim(a))
  if(is.character(projdim)){
    projdim=tolower(projdim)
    projdim=which(letters==projdim)-which(letters=="x")+1
  }
  if(ndims<3) {
    if(warn) warning("3D arrays only in zproj - no z axis in array")
    return (a)
  }
  if(!is.null(mask)) a[mask==0]=NA
  
  if(is.character(projfun) && projdim==ndims) {
    # This will do fast sums over the last dimension for 3 funcs
    projfun=match.arg(projfun)
    # These functions are handled specially
    if( projfun=="sum" || projfun=="integrate" ){
      rval=rowSums(a,dims=ndims-1,na.rm=na.rm)
    } else if(projfun=="mean"){ 		
      rval=rowMeans(a,dims=ndims-1,na.rm=na.rm)
    }
  } else {
    # This is the more general routine
    if(is.character(projfun) && projfun=="integrate") projfun.fun=sum
    else projfun.fun=match.fun(projfun)
    # Now do the projection
    margins=setdiff(1:ndims,projdim)
    rval=apply(a,margins,projfun.fun,na.rm=na.rm,...)
  }
  if(is.character(projfun) && projfun=="integrate") {
    dx=voxdim.gjdens(a)[projdim]
    rval=rval*dx
  }
  
  # copy over attributes
  attributeNamesToCopy=setdiff(names(attributes(a)),names(attributes(rval)))
  attributes(rval)=c(attributes(rval),attributes(a)[attributeNamesToCopy])
  # ... and set the ProjDim to the correct letter
  projDimChar=letters[23+projdim]
  attr(rval,'ProjDim')=if(!is.na(projDimChar)) projDimChar else projdim
  rval
}

#' Flip an array, matrix or vector about an axis
#' 
#' @param x Object to flip
#' @param ... Aditional arguments for methods
#' @export
flip<-function(x, ...) UseMethod('flip')

#' @export
#' @method flip array
#' @rdname flip
#' @param flipdim Character vector or 1-indexed integer indicating array 
#'   dimension alogn which flip will occur. Characters X, Y, Z map onto 
#'   dimensions 1, 2, 3.
#'   
#' @details Note that dimensions 1 and 2 for R matrices will be rows and 
#'   columns, respectively, which does not map easily onto the intuition of a 2D
#'   image matrix where the X axis would typically be thought of as running from
#'   left to right on the page and the Y axis would run from top to bottom.
flip.array=function(x, flipdim='X', ...){
  ndims=length(dim(x))
  if(ndims>3) stop("Can't handle more than 3D arrays")
  
  if(is.character(flipdim)){
    flipdim=tolower(flipdim)
    # fixed a bug which prevented y axis flips
    flipdim=which(letters==flipdim)-which(letters=="x")+1
  }
  if(!flipdim%in%seq(len=length(dim(x)))){
    stop("Can't match dimension to flip")
  }
  
  revidxs=dim(x)[flipdim]:1
  
  if(ndims==3){
    if(flipdim==1) rval=x[revidxs,,]
    if(flipdim==2) rval=x[,revidxs,]
    if(flipdim==3) rval=x[,,revidxs]
  }
  else if(ndims==2){
    if(flipdim==1) rval=x[revidxs,]
    if(flipdim==2) rval=x[,revidxs]
  }
  else if (ndims==1){
    return(flip.vector(x))
  }
  attributes(rval)=attributes(x)
  return (rval)
}

#' @S3method flip vector
flip.vector=function(x, ...) rev(x)

#' @S3method flip matrix
flip.matrix=function(x, ...) flip.array(x, ...)
