#' Transform the 3d location of objects such as neurons
#' 
#' \code{xform} is designed to operate on a variety of data types, especially 
#' objects encapsulating neurons.
#' 
#' @details Methods are provided for some specialised S3 classes. Further 
#'   methods can of course be constructed for user-defined S3 classes. However 
#'   this will probalbly not be necessary if the \code{xyzmatrix} and 
#'   \code{`xyzmatrix<-`} generics are suitably overloaded \emph{and} the S3 
#'   object inherits from \code{list}.
#' @details Where reg is a function, it should have a signature like
#'   \code{myfun(x, ...)} where the ... \strong{must} be provided in order to
#'   swallow any arguments passed from higher level functions that are not
#'   relevant to this particular transformation function.
#' @param x an object to transform
#' @param reg an object describing a transformation in any of the forms 
#'   understood by \code{\link{xformpoints}} (see details).
#' @param ... additional arguments passed to methods
#' @export
#' @rdname xform
#' @seealso xformpoints
xform<-function(x, reg, ...) UseMethod('xform')

#' @details TODO get this to work for matrices with more than 3 columns by
#'   working on xyzmatrix definition.
#' @method xform default
#' @param na.action How to handle NAs. NB drop may not work for some classes.
#' @rdname xform
xform.default<-function(x, reg, na.action=c('warn','none','drop','error'), ...){
  na.action=match.arg(na.action)
  pointst=xformpoints(reg, x, ...)
  if(na.action=='none') return(pointst)
  naPoints = is.na(pointst[, 1])
  if (any(naPoints)) {
    if (na.action == "drop") 
      pointst = pointst[!naPoints, ]
    else if (na.action == "warn") 
      warning("There were ", length(naPoints), " that could not be transformed")
    else if (na.action == "error") 
      stop("There were ", length(naPoints), " that could not be transformed")
  }
  pointst
}

#' @method xform list
#' @S3method xform list
#' @rdname xform
#' @param FallBackToAffine whether to use an affine transform when a cmtk
#'   warping transformation fails.
xform.list<-function(x, reg, FallBackToAffine=TRUE, na.action='error', ...){
  points=xyzmatrix(x)
  pointst=xform(points, reg, FallBackToAffine=FallBackToAffine, 
                na.action=na.action, ...)
  xyzmatrix(x)<-pointst
  x
}

#' @method xform dotprops
#' @S3method xform dotprops
#' @rdname xform
#' @details the dotprops tangent vectors will be recalculated
#'   post-transformation (even though they could in theory be transformed more
#'   or less correctly). The dotrops
#' @examples
#' \dontrun{
#' kc1=kcs20[[1]]
#' kc1.default=xform(kc1,function(x,...) x)
#' stopifnot(isTRUE(all.equal(kc1,kc1.default)))
#' kc1.5=xform(kc1,function(x,...) x, k=5)
#' stopifnot(isTRUE(all.equal(kc1.5,kc1.default)))
#' kc1.20=xform(kc1,function(x,...) x, k=20)
#' stopifnot(!isTRUE(all.equal(kc1,kc1.20)))
#' }
xform.dotprops<-function(x, reg, FallBackToAffine=TRUE, ...){
  points=xyzmatrix(x)
  pointst=xform(points, reg=reg, FallBackToAffine=FallBackToAffine, ...)
  xyzmatrix(x)=pointst
  dotprops(x, ...)
}

#' @method xform neuronlist
#' @rdname xform
xform.neuronlist<-function(x, reg, ...){
  if(length(reg)>1) stop("xform.neuronlist is currently only able to apply",
                         " a single registration to multiple neurons")
  # TODO if x is long there would be some performance benefits in chunking
  # all points from multiple neurons together. I strongly suspect that doing 10
  # at once would approach a 10x speedup.
  nlapply(x, xform, reg, ...)
}

#' Mirror 3d object about a given axis, optionally using a warping registration
#' 
#' @details The warping registration can be used to account e.g. for the
#'   asymmetry. between brain hemispheres
#'   
#' @details This function is agnostic re node vs cell data, but for node data 
#'   boundingBox should be supplied while for cell, it should be bounds See
#'   getBounds/getBoundingBox for details of bounds vs bounding box.
#' @param x Object with 3d points (with named cols X,Y,Z)
#' @param ... additional arguments passed to methods or eventually to \code{xform}
#' @return Object with transformed points
#' @export
#' @seealso \code{\link{xform},\link{getBounds}},\code{\link{getBoundingBox}}
#' @examples
#' x=Cell07PNs[[1]]
#' plot3d(x,col='red')
#' plot3d(mirror(x,168),col='green')
#' plot3d(mirror(x,168,transform='flip'),col='blue')
#' y=kcs20[[1]]
#' plot3d(y,564.2532,transform='flip',col='red')
#' plot3d(y,mirrorAxisSize=564.2532,transform='flip',col='blue')
mirror<-function(x, ...) UseMethod('mirror')

#' @param mirrorAxisSize The bounding box of the axis to mirror
#' @param mirrorAxis Axis to mirror (default \code{"X"})
#' @param warpfile Path to (optional) CMTK registration
#' @param transform whether to use warp (default) or affine component of 
#'   registration, or simply flip about midplane of axis.
#' @method mirror default
#' @S3method mirror default
#' @rdname mirror
mirror.default<-function(x, mirrorAxisSize, mirrorAxis=c("X","Y","Z"),
                         warpfile=NULL, transform=c("warp",'affine','flip'), ...){
  transform=match.arg(transform)
  mirrorAxis=match.arg(mirrorAxis)
  
  # start by flipping along mirror axis
  xyz=xyzmatrix(x)
  xyz[,mirrorAxis]=mirrorAxisSize-1*xyz[,mirrorAxis]
  xyzmatrix(x)=xyz
  
  # then 
  if(is.null(warpfile) || transform=='flip') {
    x
  } else {
    xform(x, reg=warpfile, transformtype=transform, ...)
  }
}
#' @method mirror neuronlist
#' @S3method mirror neuronlist
#' @rdname mirror
mirror.neuronlist<-function(x, ...){
  nlapply(x,mirror,...)
}
