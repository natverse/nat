#' Transform 3D points using a registration, affine matrix or function
#' 
#' @description You should almost always call \code{\link{xform}} rather calling
#'   than\code{xformpoints} directly.
#'   
#' @param reg A registration defined by a matrix, a function, a
#'   \code{\link{cmtkreg}} object, a \code{\link{reglist}} object containing a
#'   sequence of arbitrary registrations, or a character vector specifying
#'   path(s) to registrations on disk (see details).
#' @param points Nx3 matrix of points
#' @param ... Additional arguments passed to methods
#' @export
xformpoints<-function(reg, points, ...) {
  UseMethod('xformpoints')
}

#' @details If a list of transformations is passed in, these transformations are
#' performed in sequence order, such that \code{xformpoints(c(a,b,c), x) ==
#' xformpoints(c, (xformpoints(b, xformpoints(a, x))))}
#' @method xformpoints character
#' @export
#' @rdname xformpoints
xformpoints.character<-function(reg, points, ...){
    if (is.cmtkreg(reg[1], filecheck='magic')){
      reg=as.cmtkreg(reg)
    } else {
      if(all(file.exists(reg))){
        rl <- try(lapply(reg, readRDS), silent = T)
        # looks like an on disk reglist
        if(inherits( rl, 'try-error')) stop("Cannot identify registration class")
        rl=lapply(rl, reglist)
        reg = if(length(rl)==1) rl[[1]] else do.call(c, rl)
      } else {
        stop("some registrations do not seem to exist on disk")
      }
    }
  xformpoints(reg, points, ...)
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
#'   (called \strong{forward} by CMTK). Default (when \code{NULL} is inverse).
#' @param FallBackToAffine Whether to use the affine transformation for points
#'   that fail to transform under a warping transformation.
#' @export
#' @rdname xformpoints
xformpoints.cmtkreg<-function(reg, points, transformtype=c('warp','affine'),
                              direction=NULL,
                              FallBackToAffine=FALSE, ...){
  if(is.list(reg)){
    # we've been given an in memory list specifying registation parameters
    # we need to write this out to a temporary file
    regfile=as.cmtkreg(tempfile(fileext=".list"))
    on.exit(unlink(regfile,recursive=TRUE))
    write.cmtkreg(reg, regfile)
    reg=regfile
  }

  transformtype=match.arg(transformtype)
  # By default, or if swap=FALSE, we will use CMTK's inverse direction 
  if(is.null(direction)) {
    if(!is.null(swap<-attr(reg,'swap'))) {
      direction=ifelse(swap, 'forward', 'inverse')
    } else {
      direction="inverse"
    }
  } else {
    direction=match.arg(direction,c("inverse",'forward'),several.ok=TRUE)
  }
  
  if(length(reg)>1){
    # need to recycle manually
    if(length(direction)==1) direction=rep(direction, length(reg))
    if(!cmtk.version(minimum = '3.2.2')){
    # there is a bug in applying compound registrations in CMTK<=3.2.1
    # see https://github.com/jefferis/cmtk/commit/209168d892d8980e47
      for(i in seq_along(reg)) {
        points=xformpoints.cmtkreg(reg[[i]], direction=direction[[i]], points=points, transformtype=transformtype, FallBackToAffine=FallBackToAffine, ...)
      }
      return(points)
    }
  }
  # check for NAs
  nas=is.na(points[,1])
  if(sum(nas)) {
    origpoints=points
    points=points[!nas, , drop=FALSE]
  }
  pointst=cmtk.streamxform(points, reg, direction, transformtype)
  if(transformtype=='warp'){
    naPoints=is.na(pointst[,1])
    if(any(naPoints)){
      if(FallBackToAffine) {
        affpoints = xformpoints(reg,points[naPoints,,drop=FALSE],transformtype='affine')
        pointst[naPoints, ] = affpoints
      } else {
        pointst[naPoints, ] = NA_real_
      }
    }
  }

  if(sum(nas)){
    origpoints[!nas, ]=pointst
    origpoints
  } else {
    dimnames(pointst)=dimnames(points)
    pointst
  }
}

cmtk.streamxform <- function(points, reg, direction, transformtype) {
  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile))
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE)
  
  # TODO enable CMTK affine transforms using internal R code even when
  # CMTK command line tools are missing.

  # nb this -- is defensive since it may be required if the first transform
  # will be preceded by the inverse flag
  regargs="--"
  for(i in seq_along(reg)){
    regargs=c(regargs, if(direction[i]=="inverse") "--inverse" else NULL, shQuote(reg[i]))
  }
  
  outfile=tempfile()
  on.exit(unlink(outfile), add=TRUE)
  rval=cmtk.system2(cmtk.call(tool='streamxform', affine.only=transformtype=='affine', RETURN.TYPE='list'),
                    moreargs = regargs,
                    stdin=pointsfile, 
                    stdout=outfile,
                    stderr=FALSE)
  
  if(rval!=0) stop("Error running CMTK streamxform!")
  cmtkOut <- read.table(outfile,
                        col.names=c('X', 'Y', 'Z', 'Failed'), row.names=NULL,
                        colClasses=c(rep('numeric', 3), 'factor'), fill=TRUE)
  pointst <- data.matrix(cmtkOut[,1:3])
  naPoints = cmtkOut$Failed =="FAILED"
  if(any(naPoints)){
    pointst[naPoints,]=NA_real_
  }
  pointst
}

#' @export
#' @rdname xformpoints
xformpoints.reglist<-function(reg, points, ...){
  reg=simplify_reglist(reg)
  on.exit(unlinktempfiles_reglist(reg))
  # if we still have a reglist, all we can do is apply the transforms one by one
  if(inherits(reg, 'reglist')){
    for(r in reg){
      points=xformpoints(r, points, ...)
    }
    points
  } else {
    # we have a specialised structure (most likely a cmtkreg object) that we
    # should be able to apply in one shot
    xformpoints(reg, points)
  }
}

#' @method xformpoints default
#' @export
#' @rdname xformpoints
xformpoints.default<-function(reg, points, ...){
  if(!is.matrix(points) && !is.data.frame(points)) stop("points must be a matrix or dataframe")
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
