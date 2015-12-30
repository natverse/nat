#' Construct an im3d object representing 3D image data, densities etc
#' 
#' \code{im3d} objects consist of a data array with attributes defining the 
#' spatial positions at which the voxels are located. There should always be a 
#' \code{BoundingBox} attribute which defines the physical extent of the volume 
#' in the same manner as the Amira 3D visualisation and analysis software. This 
#' corresponds to the \strong{node} centers option in the 
#' \href{http://teem.sourceforge.net/nrrd/format.html}{NRRD format}.
#' @param x The object to turn into an im3d
#' @param dims The dimensions of the image array either as an integer vector 
#'   \emph{or} as an im3d object, whose attributes will provide defaults for 
#'   \code{dims, origin, BoundingBox, bounds} arguments. The default 
#'   (\code{dims=NULL}) will result in \code{dims} being set to \code{x} if 
#'   \code{x} is an \code{im3d} object or \code{dim(x)} otherwise.
#' @param voxdims The voxel dimensions
#' @param origin the location (or centre) of the first voxel
#' @param BoundingBox,bounds Physical extent of image. See the details section 
#'   of \code{\link{boundingbox}}'s help for the distinction.
#' @param ... Additional attributes such as units or materials
#' @return An array with additional class \code{im3d}
#' @details We follow Amira's convention of setting the bounding box equal to 
#'   voxel dimension (rather than 0) for any dimension with only 1 voxel.
#' @export
#' @family im3d
im3d<-function(x=numeric(0), dims=NULL, voxdims=NULL, origin=NULL,
               BoundingBox=NULL, bounds=NULL, ...){
  if(inherits(x,'im3d')){
    if(is.null(dims)) dims=x
  } else class(x)<-c("im3d",class(x))
  
  # if dims is an im3d object then use that to set 
  if(is.null(dims)) {
    dims=dim(x)
  } else if(inherits(dims,"im3d")){
    atts=attributes(dims)
    dims=dim(dims)
    if(is.null(BoundingBox)) BoundingBox=atts[["BoundingBox"]]
    if(is.null(bounds)) bounds=atts[["bounds"]]
    if(is.null(origin)) origin=atts[["origin"]]
  }
  if(length(dims)>3) stop("im3d is presently strictly limited to 3D image data")
  # add extra singleton dimension if we have 2d data
  if(length(dims)==2) dims=c(dims,1)
  # think about this - add 0 dimension if required
  if(length(voxdims)==2) voxdims=c(voxdims,0)
  boundSpecs=!c(is.null(BoundingBox), is.null(bounds), is.null(voxdims))
  if(sum(boundSpecs)<1){
    # we don't have any bounding box information, but we need to do something to 
    # indicate image dimensions
    BoundingBox=c(0,dims[1]-1,0,dims[2]-1,0,dims[3]-1)
  } else if(sum(boundSpecs)>1)
    stop("only 1 of boundingBox, bounds or voxdims can be supplied")
  if(!is.null(BoundingBox)){
    voxdims=voxdims(BoundingBox,dims=dims)
  } else if(!is.null(bounds)) {
    BoundingBox=boundingbox(bounds, dims=dims, input='bounds')
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
  # add any additional attributes
  if(!missing(...)){
    pl<-pairlist(...)
    for(n in names(pl)) attr(x,n)=pl[[n]]
  }
  x
}

#' Test if an object is of class im3d
#' @param x Object to test
#' @return logical
#' @family im3d
is.im3d<-function(x) inherits(x, 'im3d')

#' Convert a suitable object to an im3d object.
#' 
#' @details At present the only interesting method in \code{nat} is 
#'   \code{as.im3d.matrix} which can be used to convert a matrix of 3D points 
#'   into a 3D volume representation. \code{\link{ind2coord}} can be used to do 
#'   the reverse: convert a set of 3D coords to an \code{im3d} volume.
#'   
#'   Other than that, this is a largely a placeholder function with the 
#'   expectation that other packages may wish to provide suitable methods.
#' @param x Object to turn into an im3d
#' @param ... Additional arguments to pass to methods.
#' @export
#' @seealso \code{\link{im3d}}, \code{\link{ind2coord}}
#' @family im3d
as.im3d <- function(x, ...) UseMethod("as.im3d")

#' @export
#' @rdname as.im3d
as.im3d.im3d <- function(x, ...) x

#' @export
#' @param voxdims Numeric vector of length 3 \emph{or} an \code{im3d} compatible
#'   object (see details) completely specifying the required space.
#' @details \code{as.im3d.matrix} can accept any object that can be converted to
#'   an im3d object in the \code{voxdims} argument This will completely specify 
#'   the dims, voxdims, origin etc. Any value passed to those parameters will be
#'   ignored. This can be useful for producing a new im3d to match a target 
#'   image on disk or a \code{nat.templatebrains::templatebrain} object. See
#'   examples.
#' @inheritParams im3d
#' @rdname as.im3d
#' @seealso \code{\link{im3d}}, \code{\link{as.im3d}}
#' @examples
#' ## convert a list of neurons into an image volume
#' im=as.im3d(xyzmatrix(kcs20), voxdims=c(1, 1, 1), 
#'   BoundingBox=c(250, 410, 0, 130, 0, 120))
#' \dontrun{
#' write.im3d(im, 'kc20volume.nrrd')
#' 
#' ## use image dimensions of an image on disk
#' # nb note use of ReadData = FALSE so that we just fetch the dimensions of
#' # the target image
#' diskim=read.im3d("/path/to/my/image.nrrd", ReadData = FALSE)
#' im=as.im3d(xyzmatrix(kcs20), diskim)
#' 
#' ## use image dimensions of JFRC2 template brain to define the image space
#' library(nat.flybrains)
#' im=as.im3d(xyzmatrix(kcs20), JFRC2)
#' }
as.im3d.matrix<-function(x, voxdims, origin=NULL, BoundingBox=NULL, ...) {
  if(ncol(x)!=3 || nrow(x)<2) stop("Expects an Nx3 matrix of 3D points!")
  if(is.object(voxdims)){
    emptyim=try(as.im3d(voxdims))
    if(inherits(emptyim, 'try-error'))
      stop("Unable to interpret voxdims as an im3d object!",
           "It must either be an im3d or have a matching as.im3d method")
    if(!is.null(origin) || !is.null(BoundingBox))
      warning("origin and BoundingBox arguments are ignored when voxdims is ",
              "an im3d-compatible object")
    dims=dim(emptyim)
  } else {
    if(length(voxdims)!=3) stop("voxdims must have length 3")
    
    if(is.null(BoundingBox))
      r=apply(x, 2, range)
    else r=boundingbox(BoundingBox)
    
    if(is.null(origin)) origin=r[1,]
    else r[1, ]=origin
    extents=apply(r, 2, diff)
    dims=ceiling(abs(extents/voxdims))+1
    emptyim=im3d(dims = dims, voxdims = voxdims, origin=origin)
  }
  
  breaks=mapply(function(ps, delta) c(ps[1]-delta/2, ps+delta/2), 
                attributes(emptyim)[c("x","y","z")], voxdims(emptyim))
  i=cut(x[,1], breaks = breaks[[1]], labels = F)
  j=cut(x[,2], breaks = breaks[[2]], labels = F)
  k=cut(x[,3], breaks = breaks[[3]], labels = F)
  t3d=fast3dintegertable(i, j, k, dims[1], dims[2], dims[3])
  im3d(t3d, emptyim, ...)
}

fast3dintegertable<-function (a, b, c, nlevelsa = max(a), nlevelsb = max(b), 
                              nlevelsc = max(c)) {
    nlevelsabc <- nlevelsa * nlevelsb * nlevelsc
    if (nlevelsabc > .Machine$integer.max) 
        stop("Number of levels exceeds integer type.")
    inrow=nlevelsa
    inslice=nlevelsa*nlevelsb
    abc <- a + inrow * (b-1) + inslice * (c-1)
    array(tabulate(abc, nlevelsabc), dim=c(nlevelsa, nlevelsb, nlevelsc))
}

#' Read/Write calibrated 3D blocks of image data
#' 
#' @details Currently only nrrd and amira formats are implemented. Furthermore 
#'   implementing a registry to allow extension to arbitrary formats remains a 
#'   TODO item.
#'   
#'   The core attributes of an im3d object are \code{BoundingBox, origin, x, y ,
#'   z} where \code{x, y, z} are the locations of samples in the x, y and z
#'   image axes (which are assumed to be orthogonsl).
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
#' @examples
#' \dontrun{
#' # read attributes of vaa3d raw file
#' read.im3d("L1DS1_crop_straight.raw", ReadData = F, chan=2)
#' }
read.im3d<-function(file, ReadData=TRUE, SimplifyAttributes=FALSE,
                    ReadByteAsRaw=FALSE, ...){
  if(!file.exists(file)) stop("file: ", file, " doesn't exist!")
  ffs=getformatreader(file, class = 'im3d')
  if(is.null(ffs))
    stop("Unable to read data saved in format: ",tools::file_ext(file))
  
  x=match.fun(ffs$read)(file, ReadData=ReadData, ReadByteAsRaw=ReadByteAsRaw, ...)
  attr(x,'file')=file
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
#' @param format Character vector specifying an image format (e.g. "nrrd", 
#'   "amiramesh"). Optional, since the format will normally be inferred from the
#'   file extension. See \code{\link{getformatwriter}} for details.
#' @rdname im3d-io
#' @seealso \code{\link{write.nrrd}}, \code{\link{getformatwriter}}
#' @export
write.im3d<-function(x, file, format=NULL, ...){
  fw=getformatwriter(file=file, format = format, class='im3d')
  if(is.null(fw))
    stop("Unable to write data in format: ",tools::file_ext(file))
  file=fw$file
  match.fun(fw$write)(x, file=file, ...)
  invisible(file)
}

is.amiramesh.im3d<-function(f, bytes=NULL){
  rval=amiratype(f, bytes=bytes)
  sapply(rval, function(x) isTRUE(x=='uniform.field'))
}

read.im3d.amiramesh<-function(file, ReadData=TRUE, ...){
  sections = if(ReadData) NULL else NA
  d<-read.amiramesh(file, sections=sections, ...)
  
  # Amira does not store the "space origin" separately as is the case for nrrds
  # Have decided that we should always store the origin inferred from the
  # BoundingBox
  bb=attr(d,'Parameters')$BoundingBox
  origin <- if(length(bb)) bb[c(1,3,5)] else NULL
  materials <-attr(d,'Parameters')$Materials
  if(!is.null(materials)) {
    materials=data.frame(name=names(materials), id=seq_along(materials),
                         stringsAsFactors = FALSE)
  }
  im3d(d, dims=attr(d,'dataDef')$Dims[[1]], BoundingBox=bb, origin=origin,
       materials=materials)
}

read.im3d.nrrd<-function(f, ReadData=TRUE, AttachFullHeader=FALSE, 
                         ..., chan=NA){
  x=read.nrrd(file=f, ReadData = ReadData, AttachFullHeader=T, ...)
  dims=attr(x,'header')$sizes
  dims=dims[dims>1]
  if(is.na(chan)){
    if(length(dims)>3) stop("im3d is restricted to 3D image data")
  } else {
    if(ReadData)
      x=x[,,,chan]
    dims=dims[1:3]
  }
  # fetch voxel dimensions from attached header 
  h=attr(x,'header')
  voxdims=suppressWarnings(
    nrrd.voxdims(h, ReturnAbsoluteDims = FALSE))
  # drop full header if we haven't been asked for it specially
  if(!AttachFullHeader) 
    if(any(is.na(voxdims))) voxdims=NULL
  im3d(x, dims=dims, voxdims=voxdims, origin=h[['space origin']])
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

#' @export
voxdims.im3d<-function(x, ...){
  voxdims(boundingbox(x), dim(x), ...)
}

#' @export
#' @method voxdims default
#' @param dims The number of voxels in each dimension when x is a BoundingBox 
#'   matrix.
#' @rdname voxdims
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
#'   
#'   Note that there are two competing definitions for the physical extent of an
#'   image that are discussed e.g. 
#'   \url{http://teem.sourceforge.net/nrrd/format.html}. The definition that 
#'   makes most sense depends largely on whether you think of a pixel as a 
#'   little square with some defined area (and therefore a voxel as a cube with 
#'   some defined volume) \emph{or} you take the view that you can only define 
#'   with certainty the grid points at which image data was acquired. The first 
#'   view implies a physical extent which we call the  \code{bounds=dim(x) * 
#'   c(dx,dy,dz)}; the second is defined as \code{BoundingBox=dim(x)-1 * 
#'   c(dx,dy,dz)} and assumes that the extent of the image is defined by a 
#'   cuboid including the sample points at the extreme corner of the grid. Amira
#'   takes this second view and this is the one we favour given our background 
#'   in microscopy. If you wish to convert a \code{bounds} type definition into 
#'   an im3d BoundingBox, you should pass the argument \code{input='bounds'}.
#' @param x A vector or matrix specifying a bounding box, an \code{im3d} object,
#'   any object with base class list for which \code{\link{xyzmatrix}} can 
#'   extract 3D points (e.g. neurons, surfaces etc), or, for 
#'   \code{boundingbox.character}, a character vector specifying a file.
#' @inheritParams voxdims
#' @return a \code{matrix} with 2 rows and 3 columns with 
#'   \code{class='boundingbox'} or \emph{NULL} when missing.
#' @export
#' @seealso \code{\link{plot3d.boundingbox}}
#' @family im3d
#' @examples
#' boundingbox(c(x0=0,x1=10,y0=0,y1=20,z0=0,z1=30))
#' # bounding box for a neuron
#' boundingbox(Cell07PNs[[1]])
boundingbox<-function(x, ...) UseMethod("boundingbox")

#' @method boundingbox im3d
#' @export
#' @export
#' @rdname boundingbox
boundingbox.im3d<-function(x, dims=dim(x), ...) {
  atts=attributes(x)
  if(!is.null(atts$BoundingBox)) atts$BoundingBox
  else if(!is.null(atts$bounds)) {
    boundingbox(atts$bounds, dims, ...)
  } else if(isTRUE(all(c('x','y','z') %in% names(atts)))){
    # Use the locations of sample points. Note there is one special case we need
    # to consider, when dims=1 in any axis When this is the case the BoundingBox
    # found by this method will not match that determined by making the
    # calculationg using e.g. origin+voxdims.
    bb=sapply(c('x','y','z'),
                  function(d) {ll=atts[[d]];c(ll[1],ll[length(ll)])}, USE.NAMES=F)
    boundingbox(bb, dims)
  } else NULL
}

#' Return the space origin of a 3D image object
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

#' @export
#' @rdname boundingbox
boundingbox.character<-function(x, ...) {
  if(!file.exists(x))
    stop("Unable to find a file at path: ",x)
  
  boundingbox(read.im3d(x, ReadData=FALSE))
}

#' @export
#' @param na.rm Whether to ignore NA points (default \code{FALSE})
#' @description \code{boundingbox.list} is designed to be used on objects that
#'   contain 3D point information and for which \code{xyzmatrix} is defined.
#' @rdname boundingbox
boundingbox.list<-function(x, na.rm=FALSE, ...) {
  # we don't want to do this for data.frame objects
  if(is.data.frame(x)) NextMethod()
  xyz=xyzmatrix(x)
  bb=apply(xyz,2,range, na.rm=na.rm)
  boundingbox(bb)
}

#' @export
#' @description \code{boundingbox.shape3d} is designed to be used on objects 
#'   that contain 3D point information and inherit from \code{rgl}'s 
#'   \code{shape3d} class and for which \code{xyzmatrix} is defined. Presently
#'   this applies to \code{\link{mesh3d}} objects.
#'   
#' @rdname boundingbox
boundingbox.shape3d<-boundingbox.list

#' @method boundingbox default
#' @export
#' @param input Whether \code{x} defines the boundingbox or bounds of the image 
#'   (see details).
#' @rdname boundingbox
boundingbox.default<-function(x, dims, input=c("boundingbox",'bounds'), ...){
  input=match.arg(tolower(input),c("boundingbox",'bounds'))
  if(!length(x)) return(NULL)
  if(is.vector(x)) {
    if(length(x)!=6) stop("Must supply a vector of length 6")
    x=matrix(x,nrow=2)
  } else if(is.matrix(x) || is.data.frame(x)){
    if(!isTRUE(all.equal(dim(x),c(2L,3L),check.attributes=FALSE)))
      stop("Must supply a 2 x 3 matrix of physical extents")
    if(is.data.frame(x)) x=data.matrix(x)
    dimnames(x)=NULL
  }
  if(input=='bounds'){
    if(missing(dims)) stop("must supply dimensions when input is of type bounds!")
    # we need to find the voxel dimensions in order to subtract off a
    # half voxel dim in each axis
    halfVoxelDims=diff(x)/dims/2
    x[1,]=x[1,]+halfVoxelDims
    x[2,]=x[2,]-halfVoxelDims
  }
  # zap small gets rid of FP rounding errors
  structure(zapsmall(x), class = "boundingbox")
}

#' @description Set the bounding box of an im3d object
#' @rdname boundingbox
#' @param value The object which will provide the new boundingbox information.
#'   This can be be either an im3d object with a boundingbox or a vector or
#'   matrix defined according to \code{boundingbox.default}.
#' @export
`boundingbox<-`<-function(x, value) UseMethod("boundingbox<-")

#' @export
`boundingbox<-.default`<-function(x, value){
  attr(x,'BoundingBox')<-boundingbox(value)
  x
}

#' @export
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
#' @param plotdims Which dimensions of 3D \code{im3d} object to plot (character 
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
#' @param useRaster Whether to use \code{\link{rasterImage}} to plot images as a
#'   bitmap (much faster for large images). default \code{useRaster=NULL} checks
#'   \code{\link{dev.capabilities}} to see if raster images are supported.
#' @param \dots graphical parameters for \code{\link{plot}} or 
#'   \code{\link[graphics]{image}} may also be passed as arguments to this 
#'   function.
#' @return A \code{list} with elements:
#'   
#'   \itemize{
#'   
#'   \item{zlim}{ The z (intensity limits)}
#'   
#'   \item{nlevels.actual}{ The actual number of plotted levels}
#'   
#'   \item{nlevels.orig}{ The requested number of plotted levels}
#'   
#'   \item{levels}{ The chosen levels}
#'   
#'   \item{colors}{ A character vector of colours} }
#' @examples
#' \dontrun{
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
#' image(imslice(LHMask,10), asp=TRUE)
#' # useRaster is appreciably quicker in most cases
#' image(imslice(LHMask,10), asp=TRUE, useRaster=TRUE)
#' }
image.im3d<-function(x, xlim=NULL, ylim=NULL, zlim=NULL,
                     plotdims=NULL,flipdims='y', filled.contour=FALSE, asp=1,
                     axes=FALSE, xlab=NULL, ylab=NULL,
                     nlevels=20, levels = pretty(zlim, nlevels+1),
                     color.palette=colorRampPalette(c('navy','cyan','yellow','red')),
                     col = color.palette(length(levels) - 1), 
                     useRaster=NULL, ...){
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
    if(is.null(useRaster)) {
      # render images with rasters if we can
      useRaster <- FALSE
      ras <- unlist(dev.capabilities("raster"), use.names=FALSE)
      if(identical(ras, "yes")) useRaster <- TRUE
      if(identical(ras, "non-missing")) useRaster <- all(!is.na(z))
    }
    image(x=x, y=y, z=z, zlim=zlim, xlim=xlim, ylim=ylim, col=col, asp=asp,
          axes=FALSE, xlab=if(is.null(xlab)) plotdims[1] else xlab, ylab=if(is.null(xlab)) plotdims[2] else ylab, useRaster=useRaster, ...)
  }
  if(axes){
    axis(2,pretty(par("usr")[3:4]),abs(pretty(par("usr")[3:4])))
    axis(1,pretty(par("usr")[1:2]),abs(pretty(par("usr")[1:2])))
  }
  # Return info that will be useful for creating scalebars
  invisible(list(zlim=zlim,nlevels.actual=length(levels),nlevels.orig=nlevels,
                 levels=levels,colors=col))
}

#' Make 2D (orthogonal) projection of 3D image data
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
#' @seealso \code{\link{groupGeneric}}, \code{\link{clampmax}}
#' @export
#' @family im3d
#' @examples
#' \dontrun{
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
#' d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
#' op=par(mfrow=c(1,2))
#' rval=image(projection(d,projfun=max))
#' image(projection(d,projfun=clampmax(0,10)),zlim=rval$zlim)
#' par(op)
#' }
#' \dontrun{
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
#' image(projection(LHMask),asp=TRUE)
#' }
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

#' @export
flip.vector=function(x, ...) rev(x)

#' @export
flip.matrix=function(x, ...) flip.array(x, ...)

#' Slice out a 3D subarray (or 2d matrix) from a 3D image array
#' 
#' @param x An im3d objet
#' @param slice Indices defining the slices to keep
#' @param slicedim Character vector or integer defining axis from which slices 
#'   will be removed.
#' @param drop Whether singleton dimensions will be dropped (default: TRUE) 
#'   conveting 3D array to 2d matrix.
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
  # need to signal singleton dimension explicitly when it has been dropped
  newdims=dim(rval)
  if(length(newdims)<2) stop("newdims unexpectedly <2")
  if(length(newdims)==2){
    # need to insert an extra singleton dimension so that bounding box
    # is set correctly if slicedim was 1 or 2 (i.e. x or y)
    if(slicedim==1) newdims=c(1, newdims)
    else if(slicedim==2) newdims=c(newdims[1], 1, newdims[2])
  }
  rval=im3d(rval, dims=newdims, origin=origin(x), voxdims=voxdims(x))
  
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

#' Make im3d image array containing values at locations defined by a mask
#' 
#' @details The values in x will be placed into a grid defined by the dimensions
#'   of the \code{mask} in the order defined by the standard R linear 
#'   subscripting of arrays (see e.g. \code{\link{arrayInd}}).
#' @param x the data to place on a regular grid
#' @param mask An \code{im3d} regular image array where non-zero voxels are the 
#'   selected element.
#' @param default Value for regions outside the mask (default: NA)
#' @param copyAttributes Whether to copy over attributes (including \code{dim}) 
#'   from the mask to the returned object. default: \code{TRUE}
#' @param attributes. Attributes to set on new object. Defaults to attributes of
#'   \code{mask}
#' @return A new \code{im3d} object with attributes/dimensions defined by 
#'   \code{mask} and values from \code{x}. If \code{copyAttributes} is 
#'   \code{FALSE}, then it will have mode of \code{x} and length of \code{mask} 
#'   but no other attributes.
#' @export
#' @family im3d
#' @examples
#' \dontrun{
#' # read in a mask
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd', package='nat'))
#' # pick out all the non zero values
#' inmask=LHMask[LHMask!=0]
#' # fill the non-zero elements of the mask with a vector that iterates over the
#' # values 0:9
#' stripes=unmask(seq(inmask)%%10, LHMask)
#' # make an image from one slice of that result array
#' image(imslice(stripes,11), asp=TRUE)
#' }
unmask<-function(x, mask, default=NA, attributes.=attributes(mask),
                copyAttributes=TRUE){
  rval=vector(mode=mode(x),length=length(mask))
  if(copyAttributes) attributes(rval)=attributes.
  rval[mask==0]=default
  rval[mask!=0]=x
  rval
}

#' Threshold an object, typically to produce a mask
#' @param x Object to be thresholded
#' @param \dots Additional arguments passed to methods
#' @export
threshold<-function(x, ...) UseMethod("threshold")

#' @method threshold im3d
#' @param threshold Either a numeric value that pixels must \strong{exceed} in 
#'   order to be included in the mask \emph{or} a \code{logical} vector defining
#'   foreground pixels.
#' @param mode The storage mode of the resultant object (see 
#'   \code{\link{vector}}
#' @return an oject with attributes matching \code{x} and elements with value 
#'   \code{as.vector(TRUE, mode=mode)} i.e. \code{TRUE, 1, 0x01} and 
#'   \code{as.vector(FALSE, mode=mode)} i.e. \code{FALSE, 0, 0x00} as 
#'   appropriate.
#' @details Note that \code{threshold.im3d} passes \dots arguments on to im3d
#' @rdname threshold
#' @family im3d
#' @export
#' @examples
#' x=im3d(rnorm(1000),dims=c(10,10,10), BoundingBox=c(20,200,100,200,200,300))
#' stopifnot(all.equal(threshold(x, 0), threshold(x, x>0)))
threshold.im3d<-function(x, threshold=0,
                         mode=c("logical","integer","raw","numeric"), ...){
  mode=match.arg(mode)
  m=as.vector(if(is.logical(threshold)) threshold else x>threshold, mode=mode)
  im3d(m, x, ...)
}

#' Return function that finds maximum of its inputs within a clamping range
#' 
#' @details Note that by default infinite values in the input vector are 
#'   converted to \code{NA}s before the being compared with the clampmax range.
#' @param xmin,xmax clamping range. If xmax is missing xmin should be a vector 
#'   of length 2.
#' @param replace.infinite The value with which to replace non-finite values 
#'   \emph{in the input vector}. When code{replace.infinite=FALSE} no action is 
#'   taken. The default value of \code{NA} will result in e.g. \code{Inf} being 
#'   mapped to \code{NA}.
#' @return A function with signature \code{f(x, ..., na.rm)}
#' @export
#' @examples
#' \dontrun{
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
#' d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
#' op=par(mfrow=c(1,2))
#' rval=image(projection(d,projfun=max))
#' image(projection(d,projfun=clampmax(0,10)),zlim=rval$zlim)
#' par(op)
#' }
clampmax<-function(xmin, xmax, replace.infinite=NA_real_) {
  if(missing(xmax)) {
    xmax=xmin[2]
    xmin=xmin[1]
  }
  function(x, ..., na.rm=FALSE){
    if(!missing(...)) x=c(x, unlist(pairlist(...)))
    if(!is.logical(replace.infinite) || !isTRUE(!replace.infinite)){
      x[!is.finite(x)]=replace.infinite
    }
    r=suppressWarnings(max(x, na.rm=na.rm))
    if(isTRUE(r<xmin))
      xmin 
    else if(isTRUE(r>xmax))
      xmax
    else r
  }
}

#' Make a scalebar to accompany an image.im3d plot
#' 
#' @param levels The levels at which z values were cut \strong{or} a list 
#'   returned by \code{\link{image.im3d}}
#' @param col The plotted colours for each level
#' @param nlevels The number of colour levels (inferred from levels when 
#'   \code{NULL})
#' @param zlim The limits of the plotted z (intensity) values of the image
#' @param horizontal Whether to make a horizontal or vertical scalebar (default:
#'   TRUE)
#' @param lab The (single) axis label for the scale bar (default: 
#'   \code{Density})
#' @param mar The margins for ths plot
#' @param border Color for rectangle border (see \code{\link{rect}}'s 
#'   \code{border} argument for details).
#' @param \dots Additional arguments for \code{plot}
#' @export
#' @examples
#' \dontrun{
#' LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
#' op=par(no.readonly = TRUE)
#' layout(matrix(c(1, 2), ncol = 2L), widths = c(1, 0.2))
#' rval=image(imslice(LHMask,10), asp=TRUE)
#' imscalebar(rval)
#' par(op)
#' }
imscalebar<-function(levels,col,nlevels=NULL,zlim=NULL,horizontal=TRUE,lab="Density",
                       mar=c(4,2,2,2)+0.1,border=NULL, ...){
  if(!is.null(zlim) ){
    nc <- length(col)
    if ( (any(!is.finite(zlim)) || diff(zlim) < 0)) 
      stop("invalid z limits")
    if (diff(zlim) == 0) 
      zlim <- if (zlim[1] == 0) 
        c(-1, 1)
    else zlim[1] + c(-0.4, 0.4) * abs(zlim[1])
    levels=seq(from=zlim[1],to=zlim[2],len=nc+1)
  }
  # allow scaleinfo objects to be passed directly
  if(missing(col) && is.list(levels)){
    col=levels$col
    levels=levels$levels
  }
  if(horizontal){
    op=par(mar=mar)
    on.exit(par(op))
    plot(range(levels), c(0,1), type="n",
         xaxs="i", yaxs="i", xlab=lab, ylab="", yaxt="n", ...)
    rect(levels[-length(levels)], 0, border=border,
         levels[-1], col = col  , 1)
  } else {
    op=par(mar=mar[c(2,1,3,4)])
    on.exit(par(op))
    plot(c(0,1), range(levels), type="n",
         xaxs="i", yaxs="i", xlab="", ylab=lab, xaxt="n", ...)
    rect(0, levels[-length(levels)], border=border,
         1, levels[-1], col = col)
  }
}

#' Convert locations of im3d voxel grid into XYZ coordinates
#' 
#' @param d An \code{im3d} object
#' @family im3d
#' @return Nx3 matrix of image coordindates
#' @seealso expand.grid
#' @export
#' @examples
#' d=im3d(,dim=c(2,3,2),origin=c(10,20,30),voxdims=c(1,2,3))
#' imexpand.grid(d)
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
#' @name im3d-coords
#' @aliases im3d-coords xyzpos
#' @seealso \code{\link{ind2coord}}
#' @family im3d
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
#' @param roundToNearestPixel Whether to round calculated pixel coordinates to
#'   nearest integer value (i.e. nearest pixel). default: \code{TRUE}
#' @return Nx3 matrix of physica l or pixel coordinates
#' @rdname im3d-coords
#' @aliases ijkpos
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

#' Extract or set the materials for an object
#' @details Note that the id column will be the 1-indexed order that the
#'   material appears in the \code{surf$Region} list for \code{hxsurf} objects
#'   and the 0-indexed mask values for an image.
#' @param x An object in memory or, for \code{materials.character}, an image on 
#'   disk.
#' @param \dots additional parameters passed to methods (presently ignored)
#' @return A \code{data.frame} with columns \code{name, id, col}
#' @export
materials<-function(x, ...) UseMethod("materials")

#' @export
#' @rdname materials
#' @method materials default
materials.default<-function(x, ...) {
  attr(x,'materials')
}

#' @description \code{materials.character} will read the materials from an im3d 
#'   compatible image file on disk.
#' @details Presently only amiramesh images are supported since they have a
#'   standardised way of encoding labels, whereas nrrds would have to use
#'   key-value pairs according to some ad hoc convention.
#' @export
#' @rdname materials
#' @method materials character
materials.character<-function(x, ...) {
  i=read.im3d(x, ..., ReadData = FALSE)
  materials(i)
}

#' @description \code{materials.hxsurf} will extract the materials from an
#'   hxsurf object
#' @export
#' @rdname materials
#' @method materials hxsurf
#' @family hxsurf
materials.hxsurf<-function(x, ...) {
  m=data.frame(name=names(x$Regions),id=seq_along(x$Regions),
                col=x$RegionColourList, stringsAsFactors = FALSE)
  rownames(m)=m$name
  m
}

`materials<-`<-function(x, value) UseMethod("materials<-")

`materials<-.hxsurf`<-function(x, value) {
  stop("materials<-.hxsurf is not implemented")
}

`materials<-.default`<-function(x, value) {
  if(!is.data.frame(value))
    stop("materials<- expects a data.frame")
  if(!all(c("name",'id') %in% names(value)))
    stop("must supply a data.frame with columns name, id")
  attr(x,'materials') <- value
  x
}
