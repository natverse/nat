#' Transform image files using a registration or affine matrix
#'
#' @description You should almost always call \code{\link{xform}} rather
#'   calling than\code{xformimage} directly.
#'
#' @param reg A registration defined by a matrix or a \code{cmtkreg} object, or
#'   a character vector specifying a path to a CMTK registration on disk (see
#'   details). If \code{reg} is a character vector of length >1 defining a
#'   sequence of registration files on disk they should proceed from sample to
#'   reference.
#' @param image Nx3 matrix of image
#' @param ... Additional arguments passed to methods (and then eventually to
#'   \code{\link{cmtk.reformatx}})
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
#'   like to transform into template space. Here one needs CMTK's \emph{inverse}
#'   transformation.
#'
#'   A second source of confusion is that when there are multiple
#'   transformations, CMTK's reformatx tool (wrapped by
#'   \code{\link{cmtk.reformatx}}) expects them to be listed:
#'
#'   \code{ref_intermediate.list intermediate_sample.list}
#'
#'   where \code{ref_intermediate.list} is the CMTK registration obtained with
#'   ref as target/reference and intermediate as sample/floating image.
#'
#'   For consistency, all \code{xform.*} methods expect multiple registrations
#'   to be listed from sample to reference and this order is then swapped when
#'   they are passed on to \code{cmtk.reformatx}.
#'
#'   whereas CMTK's streamxform tool (wrapped by \code{\link{xformpoints}})
#'   expects them in the opposite order.
#' @param transformtype Which transformation to use when the CMTK file contains
#'   both warp (default) and affine (TODO)
#' @param direction Whether to transform image from sample space to reference
#'   space (called \strong{forward} by CMTK) or from reference to sample space
#'   (called \strong{inverse} by CMTK). Default (when \code{NULL} is forward).
#' @export
#' @rdname xformimage
#' @seealso \code{\link{cmtk.reformatx}}, \code{\link{xformpoints}},
#'   \code{\link{xform}}
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
  # By default, or if swap=FALSE, we will use CMTK's forward direction
  if(is.null(direction) && !is.null(swap<-attr(reg,'swap'))) {
    direction=ifelse(swap, 'inverse', 'forward')
  } else {
    direction=match.arg(direction,c('forward', "inverse"),several.ok=TRUE)
  }

  if(transformtype=='affine') {
    # extract affine registrations from warp
    stop("Not yet implemented")
  }
  # reverse order of multiple registrations.
  cmtk.reformatx(floating = image, registrations = rev(reg), direction=direction, ...)
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
