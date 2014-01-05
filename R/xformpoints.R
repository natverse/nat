#' Transform 3d points using a registration, affine matrix or function
#' 
#' @param reg A registration defined by a matrix, a function, a \code{cmtkreg}
#'   object, or a character vector specifying a path to a CMTK registration on
#'   disk
#' @param points Nx3 matrix of points
#' @param ... Additional arguments passed to methods
#' @export
xformpoints<-function(reg, points, ...) {
  UseMethod('xformpoints')
}

#' @details When passed a character vector, xformpoints will check to see if it
#'   defines a path containing CMTK registration erroring out if this is not the
#'   case. A future TODO would be to provide a mechanism for extending this
#'   behaviour for other registration formats.
#' @method xformpoints character
#' @rdname xformpoints
xformpoints.character<-function(reg, points, ...){
  if(is.cmtkreg(reg, filecheck='magic')) xformpoints(as.cmtkreg(reg), points, ...)
  else stop("Cannot identify registration class")
}

#' @method xformpoints cmtkreg
#' @details Note that the direction of CMTK registrations can be the source of 
#'   much confusion. This is because CMTK defines the \emph{forward} direction 
#'   as the transform required to reformat an image in \emph{sample} (floating) 
#'   space to an image in \emph{template} space. Since this operation involves 
#'   filling a regular grid in template space by looking up the corresponding 
#'   positions in sample space, the transformation that is required is (somewhat
#'   counterintuitively) the one that maps template to sample. However in 
#'   neuroanatomical work, one often has points in sample space that one would 
#'   like to transform into template space. Here one needs the \emph{inverse} 
#'   transformation.
#' @param transformtype Which transformation to use when the CMTK file contains
#'   both warp (default) and affine
#' @param direction Whether to transform points from sample space to reference 
#'   space (called \strong{inverse} by CMTK) or from reference to sample space 
#'   (called \strong{forward} by CMTK)
#' @export
#' @rdname xformpoints
xformpoints.cmtkreg<-function(reg, points, transformtype=c('warp','affine'), 
                              direction=c("inverse",'forward'), ...){
  if(is.list(reg)){
    # we've been given an in memory list specifying registation parameters
    # we need to write this out to a temporary file
    reg=tempfile(fileext=".list")
    on.exit(unlink(reg,recursive=TRUE))
    write.cmtkreg(reg)
  } else {
    # specifies warp directly
  }
  transformtype=match.arg(transformtype)
  direction=match.arg(direction)
  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile), add = TRUE)
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE)
  cmd=paste('gregxform',ifelse(direction=='forward','-f',''),
            ifelse(transformtype=='affine','-n',''),
            shQuote(reg),'<',shQuote(pointsfile))
  matrix(scan(text=system(cmd,intern = TRUE),quiet=TRUE),
         ncol=3,byrow=TRUE,
         dimnames=dimnames(points))
}

#' @method xformpoints default
#' @rdname xformpoints
xformpoints.default<-function(reg, points, ...){
  if(is.matrix(reg)) {
    points.mat=data.matrix(points)
    points[,1:3]=(cbind(points.mat,1)%*%t(reg))[,1:3]
    points
  } else if(is.function(reg)) {
    reg(points, ...)
  } else {
    stop("Unrecognised registration type")
  }
}
