#' Create and test cmtkreg objects that specify path to a CMTK registration
#'
#' @description \code{cmtkreg} creates an object of class \code{cmtkreg} that
#'   describes one (or more) \href{www.nitrc.org/projects/cmtk/}{CMTK}
#'   registrations. This is simply a character vector that also has class
#'   cmtkreg.
#' @param x Path to a cmtk registration (either plain character vector or
#'   cmtkreg object)
#' @param returnDir Whether to return the registration directory or the actual
#'   file containing the registration. When \code{returnDir=NA}, the default,
#'   returns the input path \code{x} after validation.
#' @export
cmtkreg<-function(x, returnDir=NA){
  if(length(x)>1) return(as.cmtkreg(sapply(x,cmtkreg,returnDir=returnDir, USE.NAMES = FALSE)))
  
  x=path.expand(x)
  if(!file.exists(x)) {
    return(NA_character_)
  }
  
  if(file_test("-d", x)){
    regdir=x
    # this is a directory, so see if we can find the registration
    reg=dir(x,pattern="^registration(\\.gz){0,1}",full.names=T)[1]
    if(is.na(reg)) 
      stop(paste("Unable to find registration file in",regdir))
    if(is.na(returnDir)) returnDir=TRUE
  } else {
    reg=x
    regdir=dirname(x)
    if(is.na(returnDir)) returnDir=FALSE
  }
  
  as.cmtkreg(ifelse(returnDir,regdir,reg))
}

#' @description \code{as.cmtkreg} converts objects to class \code{cmtkreg},
#'   minimally just by adding an appropriate class attribute.
#' @param ... Additional arguments passed to methods. Currently ignored.
#' @rdname cmtkreg
#' @export
as.cmtkreg<-function(x, ...) UseMethod("as.cmtkreg")


#' @rdname cmtkreg
#' @export
as.cmtkreg.matrix <- function(x, ...) {
  cmtkreglist(x, ...)
}

#' @rdname cmtkreg
#' @export
as.cmtkreg.reglist <- function(x, ...) {
  if(!all(sapply(x, is.character))) 
    stop("I cannot convert this reglist to a CMTK compatible format")
  
  # cmtkreg expects a character vector
  outseq=cmtkreg(unlist(x, use.names = F))
  swapped=as.logical(lapply(x, function(x) isTRUE(attr(x,'swap'))))
  if(any(swapped)) attr(outseq, 'swap')=swapped
  outseq
}

#' @rdname cmtkreg
#' @export
as.cmtkreg.default<-function(x, ...){
  if(!inherits(x,'cmtkreg'))
    class(x)=c("cmtkreg",class(x))
  x
}

#' @description \code{is.cmtkreg} checks if an object is a cmtk registration
#'   either by checking class (default), or inspecting file. Supports CMTK
#'   parameter files as well as NRRD deformation fields.
#' @param filecheck Whether to check object class only (default: 'none') or find
#'   and check if registration file \strong{exists} or check \strong{magic}
#'   value in first line of file.
#' @rdname cmtkreg
#' @export
is.cmtkreg<-function(x, filecheck=c('none','exists','magic')) {
  filecheck=match.arg(filecheck, choices = c('none','exists','magic'))
  if(filecheck=='none') return(inherits(x,'cmtkreg'))
  if(!is.character(x)) return (FALSE)
  
  if(length(x)>1)
    return(sapply(x, is.cmtkreg, filecheck=filecheck))
  
  reg=try(cmtkreg(x, returnDir=FALSE), silent=TRUE)
  if(inherits(reg, 'try-error') || is.na(reg)) {
    return(FALSE)
  } else if(filecheck=='exists') return(TRUE)
  # we're checking magic - make sure it implies valid file type
  !is.na(cmtkreg.filetype(reg))
}

cmtkreg.filetype <- function(x) {
  # charToRaw('! TYPEDSTREAM')
  cmtk.magic=as.raw(c(0x21, 0x20, 0x54, 0x59, 0x50, 0x45, 0x44, 0x53, 0x54, 
                      0x52, 0x45, 0x41, 0x4d))
  
  magic=try({
    gzf<-gzfile(x,'rb')
    magic<-readBin(gzf,what=raw(),n=length(cmtk.magic))
    close(gzf)
    magic},
    silent = TRUE)
  
  is_cmtk <- isTRUE(!inherits(magic, 'try-error') && 
    length(magic) == length(cmtk.magic) &&
    all(magic == cmtk.magic))
  
  if(is_cmtk)
    return("cmtk")
  
  # otherwise check if this looks like a NRRD file encoding a deformation field
  if(!is.nrrd(bytes=magic)) return(NA_character_)
  h <- read.nrrd.header(x)
  is_nrrd <- isTRUE(h$dimension==4 && h$sizes[1]==3 && h$kinds[1]=="vector")
  ifelse(is_nrrd, 'nrrd', NA_character_)
}

#' Plot the domain of a CMTK registration
#' 
#' @param x A cmtk registration (the path to the registration folder on disk) or
#'   the resulting of reading one in with \code{\link{read.cmtkreg}}.
#' @param ... Additional arguments passed to \code{\link[rgl]{plot3d}}
#' @seealso \code{\link{cmtkreg}}, \code{\link{read.cmtkreg}},
#'   \code{\link[rgl]{plot3d}}
#' @examples 
#' \donttest{
#' testdatadir=system.file("tests/testthat/testdata/cmtk", package="nat")
#' regpath=file.path(testdatadir,'FCWB_JFRC2_01_warp_level-01.list/')
#' # only run this if file is present (not always installed)
#' if(file.exists(regpath)){
#' plot3d(cmtkreg(regpath))
#' 
#' # or read registration into memory if you want to work with it
#' reg=read.cmtkreg(regpath)
#' plot3d(reg)
#' }
#' }
#' @importFrom rgl plot3d
#' @export
plot3d.cmtkreg <- function(x, ...) {
  #Handle plotting engine
  plotengine = getOption('nat.plotengine')
  if (plotengine == 'plotly') {
    plotlyreturnlist <- openplotlyscene()
  }
  
  reg=NULL
  if(is.list(x)) {
    if(!is.null(x$registration)) reg=x$registration else reg=x
    if(is.null(reg$spline_warp))
       stop("This looks like an in memory CMTK registration but I can't find the spline_warp field")
  } else if(is.character(x)) {
    reg <- read.cmtkreg(x, ReturnRegistrationOnly = TRUE)
  } else stop("Don't know what to do with this input!")
  
  coeffs=reg$spline_warp$coefficients
  aidxs=seq.int(from=1, by=3L, length.out = nrow(coeffs))
  actives=reg$spline_warp$active[aidxs]!=0
  if (plotengine == 'rgl'){
    plot3d(coeffs[actives, ], xlab='X', ylab = "Y", zlab = "Z", ...)
  } else{
    plotdata <- as.data.frame(coeffs[actives, ])
    names(plotdata) <- c('X','Y','Z')
    plotlyreturnlist$plotlyscenehandle <- plotlyreturnlist$plotlyscenehandle %>% 
                                          plotly::add_trace(data = plotdata, x = ~X, y = ~Y , z = ~Z,
                                          hoverinfo = "none",type = 'scatter3d', mode = 'markers',
                                          opacity = 1, marker=list(color = 'black', size = 3))
    plotlyreturnlist$plotlyscenehandle <- plotlyreturnlist$plotlyscenehandle %>% 
                                          plotly::layout(showlegend = FALSE, 
                                                         scene=list(camera=.plotly3d$camera))
    assign("plotlyscenehandle", plotlyreturnlist$plotlyscenehandle, envir=.plotly3d)
    print(.plotly3d$plotlyscenehandle)
    invisible(plotlyreturnlist)
  }
}
