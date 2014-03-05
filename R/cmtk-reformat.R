#' Defines a target volume for a CMTK reformatx operation
#' 
#' @details if the character vector specifies an amiramesh file, it will be
#'   converted to a bare \code{im3d} object and then to an appropriate
#'   '--target-grid' specification.
#' @param target A character vector specifying a file, an \code{im3d} object or a
#'   6-or 9-vector defining a grid in the form Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz].
#' @return a character vector specifying the full cmtk reformatx '--target' or 
#'   '--target-grid' argument
#' @export
#' @rdname cmtk.reformatx
cmtk.targetvolume<-function(target){
  if(is.character(target) && !is.nrrd(target,TrustSuffix=TRUE) &&
       isTRUE(try(is.amiramesh(target), silent=TRUE))){
    target=read.im3d(target,ReadData=FALSE)
  }
  if(is.character(target)){
    target=shQuote(target)
  } else if(is.vector(target)){
    # specify a target range c(Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz])
    if(length(target)==9) {
      target=paste("--target-grid",
                   paste(paste(target[1:3],collapse=","),paste(target[4:6],collapse=","),
                         paste(target[7:9],collapse=","),sep=":"))
    } else if(length(target)==6) {
      target=paste("--target-grid",
                   paste(paste(target[1:3],collapse=","),paste(target[4:6],collapse=","),sep=":"))
    } else stop("Incorrect target specification: ",target)
  } else if(inherits(target,'im3d')){
    # can also give a density object
    # --target-grid
    #           Define target grid for reformating as Nx,Ny,Nz:dX,dY,dZ[:Ox,Oy,Oz]
    #           (dims:pixel:origin)
    # TODO: Double check definition of origin
    target=paste("--target-grid",paste(
      paste(dim(target),collapse=","),
      paste(voxdims(target),collapse=","),
      paste(origin(target),collapse=","),sep=":")
    )
  } else {
    stop("Unrecognised target specification")
  }
  target
}

#' Refomat an image with a CMTK registration using the reformatx tool
#' 
#' @param floating The floating image to be reformatted
#' @param registrations One or more CMTK format registrations on disk
#' @param output The output image (defaults to target-floating.nrrd)
#' @param dryrun Just print command
#' @param Verbose Whether to show cmtk status messages and be verbose about file
#'   update checks. Sets \code{reformatx} \code{--verbose} option.
#' @param MakeLock Whether to use a lock file to allow simple parallelisation 
#'   (see \code{makelock})
#' @param OverWrite Whether to OverWrite an existing output file. One of 
#'   c("no","update","yes"). When OverWrite='update' 
#'   \code{\link{RunCmdForNewerInput}} is used to determine if the output is 
#'   older than any of the input files.
#' @param filesToIgnoreModTimes Input files whose modification time should not 
#'   be checked when determining if new output is required.
#' @param ... additional arguments passed to CMTK \code{reformatx} after 
#'   processing by \code{\link{cmtk.call}}.
#' @importFrom nat.utils makelock removelock RunCmdForNewerInput
#' @seealso \code{\link{cmtk.bindir}, \link{cmtk.call}, \link{makelock}, 
#'   \link{RunCmdForNewerInput}}
#' @export
cmtk.reformatx<-function(floating, target, registrations, output, 
                         dryrun=FALSE,
                         Verbose=TRUE, MakeLock=TRUE,
                         OverWrite=c("no","update","yes"),
                         filesToIgnoreModTimes=NULL, ...){
  # TODO improve default ouput file name
  if(missing(output)){
    output=file.path(dirname(floating),paste(basename(target),"-",basename(floating),'.nrrd',sep=""))
  } else if(isTRUE(file.info(output)$isdir)){
    output=file.path(output,paste(basename(target),"-",basename(floating),'.nrrd',sep=""))
  }
  if(is.logical(OverWrite)) OverWrite=ifelse(OverWrite,"yes","no")
  else OverWrite=match.arg(OverWrite)
  
  targetspec=cmtk.targetvolume(target)
  allinputs=c(floating,registrations)
  # if the target was a plain file add it to the inputs
  if(substring(targetspec,1,2)!="--") allinputs=c(allinputs,target)
  
  inputsExist=file.exists(allinputs)
  if(!all(inputsExist)){
    cat("Missing input files",basename(allinputs)[!inputsExist],"\n")
    return(FALSE)
  }
  if( file.exists(output) ){
    # output exists
    if(OverWrite=="no"){
      if(Verbose) cat("Output",output,"already exists; use OverWrite=\"yes\"\n")
      return(FALSE)
    } else if(OverWrite=="update"){
      # check modification times
      filesToCheck=setdiff(allinputs,filesToIgnoreModTimes)
    } else if(Verbose) cat("Overwriting",output,"because OverWrite=\"yes\"\n")
  } else OverWrite="yes" # just for the purpose of the runtime checks below 
  
  cmd=cmtk.call('reformatx',if(Verbose) "--verbose" else NULL,
                outfile=shQuote(output),floating=shQuote(floating),
                FINAL.ARGS=c(targetspec,paste(shQuote(registrations),collapse=" ")))
  lockfile=paste(output,".lock",sep="")
  PrintCommand<-FALSE
  if(dryrun) PrintCommand<-TRUE
  if(!dryrun) {
    if(!MakeLock) system(cmd, ignore.stderr=!Verbose, ignore.stdout=!Verbose)
    else if(makelock(lockfile)){
      if(OverWrite=="update")
        PrintCommand<-RunCmdForNewerInput(cmd,filesToCheck,output,Verbose=Verbose,...)
      else {
        PrintCommand<-TRUE;system(cmd, ignore.stderr=!Verbose, ignore.stdout=!Verbose)
      }
      removelock(lockfile)
    } else if(Verbose) cat("Unable to make lockfile:",lockfile,"\n")
  }
  if(Verbose||dryrun && PrintCommand) cat("cmd:\n",cmd,"\n") 
  return(TRUE)
}
