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
  # add extra singleton dimension if we have 2d data
  if(length(dims)==2) dims=c(dims,1)
  # think about this - add 0 dimension if required
  if(length(voxdims)==2) voxdims=c(voxdims,0)
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
    # Use the locations of sample points. Note there is one special case we need
    # to consider, when dims=1 in any axis When this is the case the BoundingBox
    # found by this method will not match that determined by making the
    # calculationg using e.g. origin+voxdims.
    bb=sapply(c('x','y','z'),
                  function(d) {ll=attr(x,d);c(ll[1],ll[length(ll)])}, USE.NAMES=F)
    boundingbox(bb, dims)
  }
}

#' Return the space origin of a 3d image object
#' 
#' @description Defined as the first coordinates (x,y,z) of the bounding box, 
#'   which in turn matches the nrrd definition of the location of the "centre" 
#'   of the first voxel.
#' @param x Object for which origin should be returned. See 
#'   \code{\link{boundingbox}}.
#' @param \dots Additional arguments passed to \code{\link{boundingbox}}
#' @family im3d
#' @export
origin<-function(x, ...) {
  boundingbox(x, ...)[c(1, 3, 5)]
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

#' @description Set the bounding box of an im3d object
#' @rdname boundingbox
#' @param value The object which will provide the new boundingbox information.
#'   This can be be either an im3d object with a boundingbox or a vector or
#'   matrix defined according to \code{boundingbox.default}.
#' @export
`boundingbox<-`<-function(x, value) UseMethod("boundingbox<-")

#' @S3method boundingbox<- default
`boundingbox<-.default`<-function(x, value){
  attr(x,'BoundingBox')<-boundingbox(value)
  x
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

#' Method to plot spatially calibrated image arrays
#' @export
#' @method image im3d
#' @importFrom graphics image .filled.contour
#' @param x The im3d object containing the data to be plotted  (\code{NA}s are 
#'   allowed).
#' @param zlim the minimum and maximum \code{z} values for which colors should 
#'   be plotted, defaulting to the range of the finite values of \code{z}. Each 
#'   of the given colors will be used to color an equispaced interval of this 
#'   range. The \emph{midpoints} of the intervals cover the range, so that 
#'   values just outside the range will be plotted.
#' @param xlim,ylim ranges for the plotted \code{x} and \code{y} values, 
#'   defaulting to the BoundingBox of x.
#' @param xlab,ylab each a character string giving the labels for the x and y 
#'   axis.  Default to the \sQuote{call names} of \code{x} or \code{y}, or to 
#'   \code{""} if these were unspecified.
#' @param plotdims Which dimensions of 3d \code{im3d} object to plot (character 
#'   vector). Defaults to \code{c('x','y')}
#' @param flipdims Which dimensions to flip (character vector). Defaults to 
#'   flipping y.
#' @param filled.contour Whether to use a \code{\link{filled.contour}} plot
#'   instead of a regular \code{\link{image}} plot.
#' @param asp Whether to have a a square aspect ratio (logical, default: 
#'   \code{FALSE})

#' @param nlevels The number of colour levels in z
#' @param levels The levels at which to break z values
#' @param axes Whether to plot axes (default: \code{FALSE})
#' @param col a list of colors such as that generated by \code{\link{rainbow}}, 
#'   \code{\link{heat.colors}}, \code{\link{topo.colors}}, 
#'   \code{\link{terrain.colors}} or similar functions.
#' @param color.palette The colour palette from which \code{col} will be 
#'   selected.
#' @param \dots graphical parameters for \code{\link{plot}} or 
#'   \code{\link{image}} may also be passed as arguments to this function. See
#'   \code{\link{image.default}}.
#' @examples
#' LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd',package='nat'))
#' image(LHMask[,,10], asp=TRUE)
#' # useRaster is appreciably quicker in most cases
#' image(LHMask[,,10], asp=TRUE, useRaster=TRUE)
image.im3d<-function(x, xlim=NULL, ylim=NULL, zlim=NULL,
                       plotdims=NULL,flipdims='y', filled.contour=FALSE, asp=NA,
                       axes=FALSE, xlab=NULL, ylab=NULL,
                       nlevels=20, levels = pretty(zlim, nlevels+1),
                       color.palette=colorRampPalette(c('navy','cyan','yellow','red')),
                       col = color.palette(length(levels) - 1), ...){
  # we will call the data object z for consistency with original image function
  z=x
  # don't try and plot anything if we have malformed zlims
  if(is.null(zlim)) zlim=range(z,finite=TRUE)
  if(!all(is.finite(zlim))){
    warning(paste("supplied zlim is not finite:",zlim))
    zlim=c(0, 0)
  }
  
  if(!is.null(plotdims)){
    plotdims=tolower(plotdims); plotdims=unlist(strsplit(plotdims,split=""))
    x=attr(z,plotdims[1])
    y=attr(z,plotdims[2])
  } else if(!is.null(attr(z,"ProjDim"))){
    # if this is a projection, then choose correct axes to display
    plotdims=setdiff(c("x","y","z"),attr(z,"ProjDim"))
    x=attr(z,plotdims[1])
    y=attr(z,plotdims[2])
  } else if (all( c("x","y")%in%names(attributes(z)) )){
    x=attr(z,"x")
    y=attr(z,"y")
  } else {
    # If we still haven't set anything, then use default
    x=seq(0,1,len=nrow(z))
    y=seq(0,1,len=ncol(z))
  }
  if(is.null(plotdims)) plotdims=c("x","y")
  # handle flipdims
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
    if(plotdims[1]%in%flipdims) z<-flip(z,1)
    if(plotdims[2]%in%flipdims) z<-flip(z,2)
  }
  
  # now reverse axes if required
  if(plotdims[1]%in%flipdims) x=-rev(x)
  if(plotdims[2]%in%flipdims) y=-rev(y)
  
  if(is.null(xlim)) xlim=range(x,finite=TRUE)
  if(is.null(ylim)) ylim=range(y,finite=TRUE)
  
  if(filled.contour){
    plot(0,0,xlim,ylim,type='n',asp=asp,axes=FALSE,
         xlab=plotdims[1],ylab=plotdims[2],xaxs="i",yaxs="i",...)
    if (!is.double(z)) storage.mode(z) <- "double"
    .filled.contour(x, y, z, levels, col = col)
  } else {
    image(x=x, y=y, z=z, zlim=zlim, xlim=xlim, ylim=ylim, col=col, asp=asp,
          axes=FALSE, xlab=plotdims[1], ylab=plotdims[2], ...)
  }
  if(axes){
    axis(2,pretty(par("usr")[3:4]),abs(pretty(par("usr")[3:4])))
    axis(1,pretty(par("usr")[1:2]),abs(pretty(par("usr")[1:2])))
  }
  # Return info that will be useful for creating scalebars
  invisible(list(zlim=zlim,nlevels.actual=length(levels),nlevels.orig=nlevels,
                 levels=levels,colors=col))
}

#' Make 2D (orthogonal) projection of 3d image data
#' 
#' @param a Array of image data (im3d format)
#' @param projdim The image dimension down which to project
#' @param projfun The function that collapses each vector of image data down to 
#'   a single pixel. Can be a character vector naming a function or a function. 
#'   See details.
#' @param na.rm Logical indicating whether to ignore \code{NA} values in the 
#'   image data when calculating function results. default: \code{TRUE}
#' @param mask A mask with the same extent as the image.
#' @param ... Additional arguments for projfun
#' @details Note that \code{projfun} must have an argument \code{na.rm} like the
#'   S3 Summary \code{\link{groupGeneric}} functions such as \code{sum, min} 
#'   etc.
#'   
#'   Note also that the BoundingBox of a 2d projection is not well-defined for
#'   the axis along which the projection was made. Presently both the evaluation
#'   location and the BoundingBox extremes are set to 0 after a projection is
#'   made but FIXME this is not completely satisfactory. Perhaps defining this
#'   to be NA or the midpoint of the orginal axis would be better justified.
#' @seealso \code{\link{groupGeneric}}
#' @export
#' @family im3d
#' @examples
#' LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd',package='nat'))
#' image(projection(LHMask),asp=TRUE)
projection<-function(a, projdim='z', projfun=c('integrate','mean','sum'), 
                     na.rm=T, mask=NULL, ...){
  ndims=length(dim(a))
  if(is.character(projdim)){
    projdim=tolower(projdim)
    projdim=which(letters==projdim)-which(letters=="x")+1
  }
  if(ndims<3)
    stop("3D arrays only in zproj - no z axis in array")

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
    dx=voxdims(a)[projdim]
    rval=rval*dx
  }
  
  # copy over attributes
  attributeNamesToCopy=setdiff(names(attributes(a)),names(attributes(rval)))
  attributes(rval)=c(attributes(rval),attributes(a)[attributeNamesToCopy])
  # ... and set the ProjDim to the correct letter
  projDimChar=letters[23+projdim]
  if(!is.na(projDimChar)){
    attr(rval,'ProjDim')=projDimChar
    attr(rval,projDimChar)=0
  } else attr(rval,'ProjDim') = projdim

  boundingbox(rval)<-NULL
  # this will use the x, y, z attributes to set the bouding box
  boundingbox(rval)<-boundingbox(rval)
  attr(rval,'OriginalBoundingBox')=attr(a,'BoundingBox')
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

#' Slice out a 3d subarray (or 2d matrix) from a 3d image array
#' 
#' @param x An im3d objet
#' @param slice Indices defining the slices to keep
#' @param slicedim Charaacter vector or integer defining axis from which slices 
#'   will be removed.
#' @param drop Whether singleton dimensions will be dropped (default: TRUE) 
#'   conveting 3d array to 2d matrix.
#' @details Note the sample locations stored in the x,y,z attributes will be 
#'   updated appropriately. FIXME: Should we also update bounding box?
#' @export
#' @family im3d
imslice<-function(x, slice, slicedim='z', drop=TRUE){
  ndims=length(dim(x))
  if(is.character(slicedim)){
    slicedim=tolower(slicedim)
    slicedim=which(slicedim==c("x",'y','z'))
  }
  if(ndims<3) {
    stop("3D arrays only in slice.gjdens - no z axis in array")
  }
  if(slicedim==3)
    rval=x[,,slice, drop=drop]
  else if(slicedim==2)
    rval=x[,slice,, drop=drop]
  else if(slicedim==1)
    rval=x[slice,,, drop=drop]
  
  class(rval)=class(x)
  # note that use of origin + voxdims to set bounding box will cope with cases
  # where there is now a singleton dimension
  rval=im3d(rval, origin=origin(x), voxdims=voxdims(x))
  
  sliceDimChar=letters[23+slicedim]
  if(!is.na(sliceDimChar)){
    attr(rval,'SliceDim')=sliceDimChar
    attr(rval,sliceDimChar)=attr(rval,sliceDimChar)[slice]
  } else{
    attr(rval,'SliceDim')=slicedim
  }
  attr(rval,'OriginalBoundingBox')=attr(x,'BoundingBox')
  rval
}

#' Check equality on data and key attributes of im3d objects
#' 
#' @inheritParams base::all.equal.default
#' @param attrsToCheck Which attributes in im3d should always be checked
#' @param attrsToCheckIfPresent Which attributes in im3d should be checked if
#'   present
#' @param CheckSharedAttrsOnly Logical whether to check shared attributes only 
#'   (default: FALSE)
#' @param ... additional arguments passed to \code{all.equal}
#' @method all.equal im3d
#' @export
#' @seealso \code{\link{all.equal}}
all.equal.im3d<-function(target, current, tolerance=1e-6,
                         attrsToCheck=c("BoundingBox"), 
                         attrsToCheckIfPresent=c('dim','names','dimnames','x','y','z'),
                         CheckSharedAttrsOnly=FALSE, ...){
  atarget=attributes(target)
  acurrent=attributes(current)
  if(length(attrsToCheck)==1 && is.na(attrsToCheck))
    fieldsToCheck=names(atarget)
  
  if(!inherits(current,'im3d'))
    return ("target and current must both be im3d objects")
  attrsInCommon=intersect(names(target),names(current))
  # figure out which of the optional fields to check are present
  attrsToCheckIfPresent=intersect(attrsInCommon,attrsToCheckIfPresent)
  # and add those to the fields to check 
  attrsToCheck=unique(c(attrsToCheck,attrsToCheckIfPresent))
  if(CheckSharedAttrsOnly){
    attrsToCheck=intersect(attrsInCommon,attrsToCheck)
  } else{
    # check all core fields
    missingfields=setdiff(attrsToCheck,names(acurrent))
    if(length(missingfields)>0)
      return(paste("Current missing attributes: ",missingfields))
    missingfields=setdiff(attrsToCheck,names(atarget))
    if(length(missingfields)>0)
      return(paste("Target missing attributes: ",missingfields))
  }
  mostattributes(target)<-atarget[attrsToCheck]
  mostattributes(current)<-acurrent[attrsToCheck]
  NextMethod(target, current, tolerance=tolerance, ...)
}

#' Make a regular im3d image array contain values at locations defined by a mask
#' 
#' @details The values in x will be placed into a grid defined by the dimensions
#'   of the \code{mask} in the order defined by the standard R linear 
#'   subscripting of arrays (see e.g. \code{\link{arrayInd}}).
#' @param x the data to place on a regular grid
#' @param mask An im3d regular image array where non-zero voxels are the 
#'   selected element.
#' @param default Value for regions outside the mask (default: NA)
#' @param copyAttributes Whether to copy over attributes (including \code{dim})
#'   from the mask to the returned object. default: \code{TRUE}
#' @param attributes. Attributes to set on new
#' @return A new \code{im3d} object with attributes/dimensions defined by 
#'   \code{mask} and values from \code{x}. If \code{copyAttributes} is 
#'   \code{FALSE}, then it will have mode of \code{x} and length of \code{mask} 
#'   but no other attributes.
#' @export
#' @family im3d
#' @examples
#' # read in a mask
#' LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd', package='nat'))
#' # pick out all the non zero values
#' inmask=LHMask[LHMask!=0]
#' # fill the non-zero elements of the mask with a vector that iterates over the
#' # values 0:9
#' stripes=unmask(seq(inmask)%%10, LHMask)
#' # make an image from one slice of that result array
#' image(imslice(stripes,11), asp=TRUE)
unmask<-function(x, mask, default=NA, attributes.=attributes(mask),
                copyAttributes=TRUE){
  rval=vector(mode=mode(x),length=length(mask))
  if(copyAttributes) attributes(rval)=attributes.
  rval[mask==0]=default
  rval[mask!=0]=x
  rval
}

#' Return function that finds maximum of its inputs within a clamping range
#' 
#' @param xmin,xmax clamping range. If xmax is missing xmin should be a vector 
#'   of length 2.
#' @return A function with signature \code{f(x, ..., na.rm)}
#' @export
#' @examples
#' LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd',package='nat'))
#' d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
#' op=par(mfrow=c(1,2))
#' rval=image(projection(d,projfun=max))
#' image(projection(d,projfun=clampmax(0,10)),zlim=rval$zlim)
#' par(op)
clampmax<-function(xmin,xmax) {
  # this fn returns a new function that will find the maximum of its inputs
  # and then clamp the return value between xmin and xmax
  # +/- Inf are converted to NA
  if(missing(xmax)) {
    xmax=xmin[2]
    xmin=xmin[1]
  }
  function(x, ..., na.rm=FALSE){
    r=suppressWarnings(max(x, ..., na.rm=na.rm))
    if(!is.finite(r))
      NA
    else if(r<xmin)
      xmin 
    else if(r>xmax)
      xmax
    else r
  }
}

#' Convert locations of im3d voxel grid into XYZ coordinates
#' 
#' @param d An \code{im3d} object
#' @return Nx3 matrix of image coordindates
#' @seealso expand.grid
imexpand.grid<-function(d){
  dims=dim(d)
  orep <- prod(dims)
  nargs=3
  
  args=list()
  bb=boundingbox(d)
  for(i in seq(dims))
    args[[i]]=seq(from=bb[1,i],to=bb[2,i],length=dims[i])
  
  rep.fac <- 1
  rval=matrix(nrow=orep,ncol=length(dims))
  for (i in 1:nargs) {
    x <- args[[i]]
    nx <- length(x)
    orep <- orep/nx
    x <- x[rep.int(rep.int(seq(length = nx), rep.int(rep.fac, 
                                                     nx)), orep)]
    if (!is.factor(x) && is.character(x)){
      cat("converting to factor")
      x <- factor(x, levels = unique(x))
    }
    rval[,i]=x
    rep.fac <- rep.fac * nx
  }
  return(rval)
}

#' Interconvert pixel and physical coordinates
#' @description \code{xyzpos} converts pixel coordinates to physical coordinates
#' @param d An \code{im3d} object defining a physical space
#' @param ijk an Nx3 matrix of pixel coordinates (1-indexed)
#' @rdname im3d-xyzijk
#' @export
xyzpos<-function(d, ijk)
{
  # transpose if we have received a matrix (with 3 cols i,j,k) so that
  # multiplication below does not need to be changed
  if(is.matrix(ijk)) ijk=t(ijk)
  if(any(ijk<1)) warning("expects 1-indexed pixel coordinate so pixels <1 make little sense")
  xyz=(ijk-1)*voxdims(d)+origin(d)
  if(is.matrix(xyz)) t(xyz) else xyz
}

#' @description \code{ijkpos} converts physical coordinates to pixel coordinates
#' @param xyz Nx3 matrix of physical coordinates
#' @param roundToNearestPixel
#' @return Nx3 matrix of physica l or pixel coordinates
#' @rdname im3d-xyzijk
#' @export
#' @examples
#' # make an emty im3d
#' d=im3d(,dim=c(20,30,40),origin=c(10,20,30),voxdims=c(1,2,3))
#' # check round trip for origin
#' stopifnot(all.equal(ijkpos(d,xyzpos(d,c(1,1,1))), c(1,1,1)))
ijkpos<-function(d, xyz, roundToNearestPixel=TRUE)
{
  # return the ijk position for a physical location (x,y,z)
  # This will be the pixel centre based on the bounding box
  # Note that ijk will be 1-indexed according to R's convention
  
  # transpose if we have received a matrix (with 3 cols x,y,z) so that
  # multiplication below doesn not need to be changed
  if(is.matrix(xyz)) xyz=t(xyz)
    
  ijk=(xyz-origin(d))/voxdims(d)+1
  if(roundToNearestPixel) {
    ijk=round(ijk)
    if(any(ijk<1) || any(ijk>dim(d))) warning("pixel coordinates outside image data")
  }
  if(is.matrix(ijk)) t(ijk) else ijk
}
