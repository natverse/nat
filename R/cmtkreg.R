#' Create and test cmtkreg objects that specify path to a CMTK registration
#' 
#' @description \code{cmtkreg} creates an object of class \code{cmtkreg} that
#'   describes one (or more) \href{www.nitrc.org/projects/cmtk/}{CMTK}
#'   registrations. This is simply a character vector that also has class
#'   cmtkreg.
#' @param x Path to a cmtk registration (either plain character vector or 
#'   cmtkreg object)
#' @param returnDir Whether to return the registration directory (default) or 
#'   the actual file containing the registration
#' @export
cmtkreg<-function(x, returnDir=TRUE){
  if(length(x)>1) return(sapply(x,cmtkreg,returnDir=returnDir))
  
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
  } else {
    reg=x
    regdir=dirname(x)
  }
  
  as.cmtkreg(ifelse(returnDir,regdir,reg))
}

#' @description \code{as.cmtkreg} adds class \code{cmtkreg} to objects that do not
#'   already inherit from it.
#' @rdname cmtkreg
#' @export
as.cmtkreg<-function(x){
  if(!inherits(x,'cmtkreg'))
    class(x)=c("cmtkreg",class(x))
  x
}

#' @description \code{is.cmtkreg} checks if an object is a cmtk registration
#'   either by checking class (default), or inspecting file.
#' @param filecheck Whether to check object class only (default: 'none') or find
#'   amd check if registration file \strong{exists} or check \strong{magic} 
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
  
  # charToRaw('! TYPEDSTREAM')
  cmtk.magic=as.raw(c(0x21, 0x20, 0x54, 0x59, 0x50, 0x45, 0x44, 0x53, 0x54, 
                      0x52, 0x45, 0x41, 0x4d))
  
  magic=try({
    gzf<-gzfile(reg,'rb')
    magic<-readBin(gzf,what=raw(),n=length(cmtk.magic))
    close(gzf)
    magic},
    silent = TRUE)

  return(!inherits(magic,'try-error') && 
           length(magic)==length(cmtk.magic) 
         && all(magic==cmtk.magic))
}
