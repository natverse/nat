#' Transform image files using a registration or affine matrix
#' 
#' @param reg A registration defined by a matrix or a \code{cmtkreg} object, or
#'   a character vector specifying a path to a CMTK registration on disk (see
#'   details).
#' @param image Nx3 matrix of image
#' @param ... Additional arguments passed to methods
#' @return Character vector with path to xformed image.
#' @export
xformimage<-function(reg, image, ...) {
  UseMethod('xformimage')
}

#' @details When passed a character vector, xformimage will check to see if it 
#'   defines a path containing CMTK registration erroring out if this is not the
#'   case. If the path does indeed point to a CMTK registration, this method
#'   will hand off to xformimage.cmtkreg. A future TODO would be to provide a
#'   mechanism for extending this behaviour for other registration formats.
#
#'   If a list of transformations is passed in, these transformations are passed
#'   to the cmtk reformatx tool in the order received. Note that there is
#'   presently no support for \itemize{
#'   
#'   \item using the inverse of a registration
#'   
#'   \item specifying a mask
#'   
#'   \item passing additional arguments to reformatx
#'   
#'   }
#'   
#' @export
#' @rdname xformimage
xformimage.character<-function(reg, image, ...){
    if (is.cmtkreg(reg[1], filecheck='magic')) xformimage(as.cmtkreg(reg), image, ...)
    else stop("Cannot identify registration class")
}

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
#'   both warp (default) and affine (TODO)
#' @param direction Whether to transform image from sample space to reference 
#'   space (called \strong{inverse} by CMTK) or from reference to sample space 
#'   (called \strong{forward} by CMTK)
#' @export
#' @rdname xformimage
#' @seealso \code{\link{cmtk.reformatx}}
xformimage.cmtkreg<-function(reg, image, transformtype=c('warp','affine'), 
                              direction=NULL, ...){
  if(is.list(reg)){
    # we've been given an in memory list specifying registation parameters
    # we need to write this out to a temporary file
    regfile=as.cmtkreg(tempfile(fileext=".list"))
    on.exit(unlink(regfile,recursive=TRUE))
    write.cmtkreg(reg, regfile)
    reg=regfile
  }
  
  transformtype=match.arg(transformtype)
  direction=match.arg(direction,c('forward', "inverse"),several.ok=TRUE)
  if(any('inverse'%in%direction))
    stop("cmtk.reformatx does not handle application of inverse registrations")
  if(transformtype=='affine') {
    # extract affine registrations from warp
    stop("Not yet implemented")
  }
  cmtk.reformatx(floating = image, registrations = reg, ...)
}

#' @export
#' @rdname xformimage
xformimage.default<-function(reg, image, ...){
  if(is.matrix(reg)) {
    # convert to a CMTK registration params (in memory)
    xformimage(cmtkreglist(reg), image, ...)
  } else {
    stop("Unrecognised/implemented registration type")
  }
}
