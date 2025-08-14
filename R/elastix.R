elastixreg <- function(x, ...) {
  if(!inherits(x,'elastixreg'))
    class(x)=c("elastixreg",class(x))
  x
}

#' @export
#' @rdname xformpoints
xformpoints.elastixreg<-function(reg, points, transformtype=c('warp','affine'),
                              direction=NULL, ...) {
  if(is.list(reg)){
    stop("transformation using the in memory representation is not yet supported")
  }
  
  # By default, or if swap=FALSE, we will use elastix's inverse direction 
  if(is.null(direction)) {
    if(!is.null(swap<-attr(reg,'swap'))) {
      direction=ifelse(swap, 'forward', 'inverse')
    } else {
      direction="inverse"
    }
  } else {
    direction=match.arg(direction,c("inverse",'forward'), several.ok=TRUE)
  }
  
  if (length(reg) > 1) {
    # need to recycle manually
    if (length(direction) == 1)
      direction = rep(direction, length(reg))
    for (i in seq_along(reg)) {
      points = xformpoints.elastixreg(reg[[i]], direction = direction[[i]], points =
                                        points, ...)
    }
    return(points)
  }
  # check for NAs
  nas=is.na(points[,1])
  if(sum(nas)) {
    origpoints=points
    points=points[!nas, , drop=FALSE]
  }
  rawout=transformix.points(points, reg)
  pointst=xyzmatrix(rawout)
  # if(transformtype=='warp'){
  #   naPoints=is.na(pointst[,1])
  #   if(any(naPoints)){
  #     if(FallBackToAffine) {
  #       stop("FallBackToAffine not currently supported for elastix transforms!")
  #       affpoints = xformpoints(reg, points[naPoints,,drop=FALSE],transformtype='affine')
  #       pointst[naPoints, ] = affpoints
  #     } else {
  #       pointst[naPoints, ] = NA_real_
  #     }
  #   }
  # }
  
  # if(sum(nas)){
  #   origpoints[!nas, ]=pointst
  #   origpoints
  # } else {
  #   dimnames(pointst)=dimnames(points)
  #   pointst
  # }
  dimnames(pointst)=dimnames(points)
  pointst
}

# function to handle the details of calling the transformix command line tool
# nb reg must be a specific transform file (not a directory)
transformix.points <- function(points, reg, stderr=FALSE) {
  pointsfile=tempfile(fileext=".txt")
  on.exit(unlink(pointsfile))
  outdir=tempfile()
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  
  writeLines(text = c("point", nvertices(points)), con = pointsfile)
  write.table(points, file=pointsfile, row.names=FALSE, col.names=FALSE, append = TRUE)
  
  rval <- elastix.call('transformix', out=outdir, tp=reg, def=pointsfile)
  
  if(rval!=0) 
    stop("Error running transformix!",
         "See log file in output directory for details:\n", outdir)
  
  outfile=dir(outdir, pattern = '\\.txt$', full.names = TRUE)
  if(isFALSE(length(outfile)==1))
    stop("transformix error: could not file exactly one output file in ",
         "the directory:", outdir)
  transformixOut <- read.table(outfile, row.names=NULL)
  rawout <- transformixOut[c(2,7:9,15:17,23:25,31:33, 39:41)]
  colnames(rawout) <- c("id", "Ii", "Ij", "Ik", "Ix", "Iy","Iz", 
                                "i", "j", "k", "x", "y", "z", "Dx", "Dy", "Dz")
  rawout
}

elastix.call <- function(tool=c("transformix", "elastix"), ..., 
                         PROCESSED.ARGS=NULL, 
                         FINAL.ARGS=NULL, stderr=FALSE, stdout=FALSE){
  tool=match.arg(tool)
  cmd=file.path(elastix.bindir(check=TRUE), tool)
  elastixargs=character()
  
  if(!is.null(PROCESSED.ARGS)){
    elastixargs=c(elastixargs, PROCESSED.ARGS)
  }
  
  if(!missing(...)){
    xargs=pairlist(...)
    for(n in names(xargs)){
      arg=xargs[[n]]
      elastixarg=elastix.arg.name(n)
      if(is.character(arg)){
        if(length(arg)!=1) stop("character arguments must have length 1")
        elastixargs=c(elastixargs, elastixarg, arg)
      } else if(is.logical(arg)){
        if(isTRUE(arg)) elastixargs=c(elastixargs, elastixarg)
      } else if(is.numeric(arg)){
        arg=paste(arg, collapse=',')
        elastixargs=c(elastixargs, elastixarg, arg)
      } else if(is.null(arg)){
        # just ignore null arguemnts
      } else {
        stop("unrecognised argument type")
      }
    }
  }
  
  if(!is.null(FINAL.ARGS)){
    elastixargs=c(elastixargs, FINAL.ARGS)
  }
  call=list(command=cmd, args=elastixargs)
  cmtk.system2(call, stderr=stderr, stdout=stdout)
}

elastix.arg.name<-function(x) {
  # elastix sometimes uses -- but mostly uses -
  prefix <- if(x %in% c("version", "extended.version", "help")) "--" else "-"
  paste(prefix, gsub("\\.", '-', x), sep='')
}

elastix.bindir<-function(firstdir=getOption('nat.elastix.bindir'),
                      extradirs=c('~/bin', '/usr/local/bin','/opt/local/bin'),
                      set=FALSE, check=FALSE, elastixtool='transformix'){
  # TODO check pure Windows vs cygwin
  if(isTRUE(.Platform$OS.type=="windows" && tools::file_ext(elastixtool)!="exe"))
    cmtktool=paste0(elastixtool,".exe")
  bindir=NULL
  if(!is.null(firstdir)) {
    bindir=firstdir
    if(check && !file.exists(file.path(bindir,elastixtool)))
      stop("cmtk is _not_ installed at:", bindir,
           "\nPlease check value of options('nat.elastix.bindir')")
  }
  # note the use of while loop + break to avoid heavy if nesting
  while(is.null(bindir)){
    if(nzchar(cmtktoolpath<-Sys.which(elastixtool))){
      bindir=dirname(cmtktoolpath)
      break
    }
    # otherwise check some plausible locations
    for(d in extradirs){
      if(file.exists(file.path(d, elastixtool))) {
        bindir=d
        break
      }
    }
    # we're out of luck but we still need to break out of the while loop
    break
  }
  if(!is.null(bindir)){
    if(is.na(bindir)) bindir=NULL
    else bindir=path.expand(bindir)
  }
  if(check && is.null(bindir))
    stop("Cannot find Elastix. Please install from ",
         "http://elastix.isi.uu.nl and make sure that it is in your path!")
  
  if(set) {
    options(nat.elastix.bindir=bindir)
  }
  bindir
}

plot3d.elastixreg <- function(x, type=c("p","l","b"), ..., plotengine = getOption('nat.plotengine')) {
  plotengine <- check_plotengine(plotengine)
  if (plotengine == 'plotly') {
    psh <- openplotlyscene()$plotlyscenehandle
    params=list(...)
    opacity <- if("alpha" %in% names(params)) params$alpha else 1
  }
  
  reg=NULL
  if(is.list(x)) {
    if(is.null(x$TransformParameters))
      stop("This looks like an in memory elastix registration but I can't find the TransformParameters field!")
  } else if(is.character(x)) {
    x <- read.elastixreg(x)
  } else stop("Don't know what to do with this input!")
  
  type=match.arg(type)

    # make a fake im3d object using the grid information
  gridim=im3d(dims = x$GridSize, voxdims = x$GridSpacing, origin = x$GridOrigin)
  # now make an Nx3 matrix 
  grid=imexpand.grid(gridim)
  if(!isTRUE(all.equal(dim(grid), dim(x$TransformParameters))))
    stop("Mismatch between transform parameters and expected grid size!")
  gridt=grid+x$TransformParameters
  
  if (plotengine == 'rgl'){
    if(type %in% c("b", "p")) {
      plot3d(gridt, xlab='X', ylab = "Y", zlab = "Z", ...)
    } else {
      plot3d(gridt, xlab='X', ylab = "Y", zlab = "Z", type='n', ...)
    }
    interleaved=matrix(t(cbind(grid,gridt)),ncol=3,byrow=T)
    if(type %in% c('l','b')) {
      segments3d(interleaved, ...)
    }
    
  } else{
    plotdata <- as.data.frame(gridt)
    names(plotdata) <- c('X','Y','Z')
    psh <- psh %>% 
      plotly::add_trace(data = plotdata, x = ~X, y = ~Y , z = ~Z,
                        hoverinfo = "none",type = 'scatter3d', mode = 'markers',
                        opacity = opacity, marker=list(color = 'black', size = 3)) %>% 
      plotly::layout(showlegend = FALSE, scene=list(camera=.plotly3d$camera))
    assign("plotlyscenehandle", psh, envir=.plotly3d)
    psh
  }
}
