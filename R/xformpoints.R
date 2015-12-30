#' Transform 3D points using a registration, affine matrix or function
#' 
#' @description You should almost always call \code{\link{xform}} rather
#'   calling than\code{xformpoints} directly.
#'   
#' @param reg A registration defined by a matrix, a function, a \code{cmtkreg} 
#'   object, or a character vector specifying a path to a CMTK registration on 
#'   disk (see details).
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
    if (is.cmtkreg(reg[1], filecheck='magic')) xformpoints(as.cmtkreg(reg), points, ...)
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
  if(is.null(direction) && !is.null(swap<-attr(reg,'swap'))) {
    direction=ifelse(swap, 'forward', 'inverse')
  } else {
    direction=match.arg(direction,c("inverse",'forward'),several.ok=TRUE)
  }
  
  
  if(length(reg)>1 && !cmtk.version(minimum = '3.2.2')){
    # there is a bug in applying compound registrations in CMTK<=3.2.1
    # see https://github.com/jefferis/cmtk/commit/209168d892d8980e47
    if(length(direction)==1) direction=rep(direction, length(reg))
    for(i in seq_along(reg)) {
      points=xformpoints.cmtkreg(reg[[i]], direction=direction[[i]], points=points, transformtype=transformtype, FallBackToAffine=FallBackToAffine, ...)
    }
    return(points)
  }

  # check for NAs
  nas=is.na(points[,1])
  if(sum(nas)) {
    origpoints=points
    points=points[!nas, , drop=FALSE]
  }

  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile), add = TRUE)
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE)
  streamxform=file.path(cmtk.bindir(check=TRUE),'streamxform')
  # TODO enable CMTK affine transforms using internal R code even when
  # CMTK command line tools are missing.
  inverseflags <- unlist(lapply(direction, function(x) ifelse(x == 'forward', '', '--inverse')))
  regcmd <- paste(c(rbind(inverseflags, shQuote(path.expand(reg)))), collapse=" ")
  outfile=tempfile()
  on.exit(unlink(outfile), add=TRUE)
  cmd=paste(streamxform,ifelse(transformtype=='affine','--affine-only',''), '--',
            regcmd,'<',shQuote(pointsfile),">",shQuote(outfile))
  if(system(cmd,ignore.stderr=TRUE)!=0) stop("Error running CMTK streamxform!")
  cmtkOut <- read.table(outfile,
                        col.names=c('X', 'Y', 'Z', 'Failed'), row.names=NULL,
                        colClasses=c(rep('numeric', 3), 'factor'), fill=TRUE)
  pointst <- data.matrix(cmtkOut[,1:3])

  if(transformtype=='warp'){
    naPoints = cmtkOut$Failed =="FAILED"
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
