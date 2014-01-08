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
  nlapply(x, xform, reg, ...)
}
