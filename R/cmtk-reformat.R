# Defines a target volume for a CMTK reformatx operation
# 
# @details if the character vector specifies an amiramesh file, it will be
#   converted to a bare \code{im3d} object and then to an appropriate
#   '--target-grid' specification.
# @param target A character vector specifying a file, an \code{im3d} object or a
#   6-or 9-vector defining a grid in the form Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz].
# @return a character vector specifying the full cmtk reformatx '--target' or 
#   '--target-grid' argument
cmtk.targetvolume<-function(target){
  if(is.character(target) && is.amiramesh(target)){
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
    target=paste("--target-grid",shQuote(paste(
      paste(dim(target),collapse=","),
      paste(voxdims(target),collapse=","),
      paste(origin(target),collapse=","),sep=":")
    ))
  } else {
    stop("Unrecognised target specification")
  }
  target
}

ReformatImage<-function(floating,target,registrations,output, 
                        dryrun=FALSE, Verbose=TRUE, MakeLock=TRUE, OverWrite=c("no","update","yes"),
                        filesToIgnoreModTimes=NULL,
                        reformatxPath=file.path(cmtk.bindir(check=TRUE),"reformatx"),reformatoptions="-v --pad-out 0",
                        Push=FALSE,...){
  # TODO improve default ouput file name
  if(missing(output)){
    output=file.path(dirname(floating),paste(basename(target),"-",basename(floating),'.nrrd',sep=""))
  } else if(isTRUE(file.info(output)$isdir)){
    output=file.path(output,paste(basename(target),"-",basename(floating),'.nrrd',sep=""))
  }
  if(is.logical(OverWrite)) OverWrite=ifelse(OverWrite,"yes","no")
  else OverWrite=match.arg(OverWrite)
  
  targetspec=.makeReformatxTargetSpecification(target)
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
  
  cmd=paste(shQuote(reformatxPath), reformatoptions,
            "-o",shQuote(output),ifelse(Push,"--push",""),"--floating",shQuote(floating),targetspec,
            paste(shQuote(registrations),collapse=" "))
  lockfile=paste(output,".lock",sep="")
  PrintCommand<-FALSE
  if(dryrun) PrintCommand<-TRUE
  if(!dryrun) {
    if(!MakeLock) system(cmd,...)
    else if(makelock(lockfile)){
      if(OverWrite=="update")
        PrintCommand<-RunCmdForNewerInput(cmd,filesToCheck,output,Verbose=Verbose,...)
      else {
        PrintCommand<-TRUE;system(cmd,...)
      }
      removelock(lockfile)
    } else if(Verbose) cat("Unable to make lockfile:",lockfile,"\n")
  }
  if(PrintCommand) cat("cmd:\n",cmd,"\n") 
  return(TRUE)
}
