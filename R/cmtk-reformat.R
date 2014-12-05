#' Defines a target volume for a CMTK reformatx operation
#' 
#' @details if the character vector specifies an amiramesh file, it will be 
#'   converted to a bare \code{im3d} object and then to an appropriate 
#'   '--target-grid' specification.
#' @param target A character vector specifying a file, an \code{im3d} object or
#'   a 6-or 9-vector defining a grid in the form Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz].
#' @param ... additional arguments passed to methods
#' @return a character vector specifying the full cmtk reformatx '--target' or 
#'   '--target-grid' argument
#' @export
cmtk.targetvolume<-function(target, ...) UseMethod("cmtk.targetvolume")

#' @export
#' @rdname cmtk.targetvolume
cmtk.targetvolume.im3d<-function(target, ...) {
  cmtk.targetvolume(c(dim(target), voxdims(target), origin(target)))
}

#' @description \code{cmtk.targetvolume.list} is designed to cope with any 
#'   user-defined class for which an as.im3d method exists. Presently the only
#'   example in the nat.* ecosystem is
#'   \code{nat.templatebrains::as.im3d.templatebrain}.
#' @export
#' @rdname cmtk.targetvolume
#' @examples
#' \dontrun{
#' # see https://github.com/jefferislab/nat.flybrains
#' library(nat.flybrains)
#' cmtk.targetvolume(FCWB)
#' }
cmtk.targetvolume.list<-function(target, ...) {
  cmtk.targetvolume(as.im3d(target))
}

#' @export
#' @rdname cmtk.targetvolume
cmtk.targetvolume.default<-function(target, ...) {  
  if(is.character(target) && !is.nrrd(target,TrustSuffix=TRUE) &&
       isTRUE(try(is.amiramesh(target), silent=TRUE))){
    return(cmtk.targetvolume(read.im3d(target,ReadData=FALSE)))
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
  } else {
    stop("Unrecognised target specification")
  }
  target
}

#' Reformat an image with a CMTK registration using the reformatx tool
#' 
#' @details Note that if you are reformatting a mask then you will need to 
#'   change the interpolation to "nn", since interpolating between e.g. mask 
#'   levels 72 and 74 with 73 may have unintened consequences. Presently we have
#'   no way of knowing whether an image should be treated as a mask, so the 
#'   \code{interpolation} must be handled manually.
#' @param floating The floating image to be reformatted
#' @param registrations One or more CMTK format registrations on disk
#' @param output The output image (defaults to targetstem-floatingstem.nrrd)
#' @param mask Whether to treat target as a binary mask (only reformatting
#'   positve voxels)
#' @param interpolation What interpolation scheme to use for output image 
#'   (defaults to linear - see details)
#' @param dryrun Just print command
#' @param Verbose Whether to show cmtk status messages and be verbose about file
#'   update checks. Sets command line \code{--verbose} option.
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
#' @inheritParams cmtk.targetvolume
#' @importFrom nat.utils makelock removelock RunCmdForNewerInput
#' @seealso \code{\link{cmtk.bindir}, \link{cmtk.call}, \link{makelock}, 
#'   \link{RunCmdForNewerInput}}
#' @export
#' @return the path to the ouput image (whether or not it was re-created afresh)
#'   or \code{NA_character_} if no output was possible.
#' @examples
#' \dontrun{
#' cmtk.reformatx('myimage.nrrd', target='template.nrrd',
#'   registrations='template_myimage.list')
#' 
#' # get full listing of command line options  
#' system(cmtk.call('reformatx', help=TRUE))
#' }
cmtk.reformatx<-function(floating, registrations, output, target, mask=FALSE,
                         interpolation=c("linear", "nn", "cubic", "pv", "sinc-cosine", "sinc-hamming"),
                         dryrun=FALSE, Verbose=TRUE, MakeLock=TRUE, 
                         OverWrite=c("no","update","yes"),
                         filesToIgnoreModTimes=NULL, ...){
  # TODO improve default ouput file name
  basestem<-function(f) tools::file_path_sans_ext(basename(f))
  if(missing(output)){
    output=file.path(dirname(floating),paste(basestem(target),"-",basestem(floating),'.nrrd',sep=""))
  } else if(isTRUE(file.info(output)$isdir)){
    output=file.path(output,paste(basestem(target),"-",basestem(floating),'.nrrd',sep=""))
  }
  if(is.logical(OverWrite)) OverWrite=ifelse(OverWrite,"yes","no")
  else OverWrite=match.arg(OverWrite)
  
  interpolation=match.arg(interpolation)
  
  targetspec=cmtk.targetvolume(target)
  allinputs=c(floating,registrations)
  # if the target was a plain file add it to the inputs
  if(substring(targetspec,1,2)!="--") allinputs=c(allinputs,target)
  
  inputsExist=file.exists(allinputs)
  if(!all(inputsExist)){
    cat("Missing input files",basename(allinputs)[!inputsExist],"\n")
    return(NA_character_)
  }
  if( file.exists(output) ){
    # output exists
    if(OverWrite=="no"){
      if(Verbose) cat("Output",output,"already exists; use OverWrite=\"yes\"\n")
      return(output)
    } else if(OverWrite=="update"){
      # check modification times
      filesToCheck=setdiff(allinputs,filesToIgnoreModTimes)
    } else if(Verbose) cat("Overwriting",output,"because OverWrite=\"yes\"\n")
  } else OverWrite="yes" # just for the purpose of the runtime checks below 
  
  cmd=cmtk.call('reformatx',if(Verbose) "--verbose" else NULL,
                outfile=shQuote(output),floating=shQuote(floating),
                mask=mask, interpolation=interpolation, ...,
                FINAL.ARGS=c(targetspec,paste(shQuote(registrations),collapse=" ")))
  lockfile=paste(output,".lock",sep="")
  PrintCommand<-FALSE
  if(dryrun) PrintCommand<-TRUE
  if(!dryrun) {
    if(!MakeLock) system(cmd, ignore.stderr=!Verbose, ignore.stdout=!Verbose)
    else if(makelock(lockfile)){
      if(OverWrite=="update")
        PrintCommand<-RunCmdForNewerInput(cmd,filesToCheck,output,Verbose=Verbose)
      else {
        PrintCommand<-TRUE;system(cmd, ignore.stderr=!Verbose, ignore.stdout=!Verbose)
      }
      removelock(lockfile)
    } else if(Verbose) cat("Unable to make lockfile:",lockfile,"\n")
  }
  if(Verbose||dryrun && PrintCommand) cat("cmd:\n",cmd,"\n") 
  return(output)
}

#' Calculate image statistics for a nrrd or other CMTK compatible file
#' 
#' @details When given a label mask returns a dataframe with a row for each 
#'   level of the label field. If GJ's modified version of CMTK statistics is 
#'   available this will include an extra column with the number of non-zero 
#'   voxels in the main image for each level of the mask.
#' @details Note that the Entropy column (sometimes H, sometimes Entropy) will 
#'   always be named Entropy in the returned dataframe.
#' @param f Path to image file (any CMTK compatible format)
#' @param mask Optional path to a mask file
#' @param masktype Whether mask should be treated as label field or binary mask 
#'   (default label)
#' @param ... Additional arguments for ctmk's statistics tool processed by 
#'   \code{\link{cmtk.call}}.
#' @inheritParams cmtk.reformatx
#' @return return dataframe describing results
#' @export
#' @examples
#' \dontrun{
#' cmtk.statistics('someneuron.nrrd',mask='neuropilregionmask.nrrd')
#' }
cmtk.statistics<-function(f, mask, masktype=c("label", "binary"), ..., Verbose=FALSE){
  masktype=match.arg(masktype)
  if(length(f)>1) return(sapply(f,cmtk.statistics,mask=mask,masktype=masktype, ...))
  args=f
  if(!missing(mask)){
    args=c(ifelse(masktype=='label','--Mask','--mask'), mask, args)
  }
  cmd=cmtk.call("statistics", PROCESSED.ARGS = if(Verbose) "--verbose" else NULL, 
                FINAL.ARGS = args, ... = ...)
  rval=system(cmd, intern = TRUE, ignore.stderr=!Verbose)
  # there is a bug in versions of CMTK statistics <2.3.1 when used with a mask 
  # the header says that there are two entropy columns (H1,H2)
  # but in fact there is only 1. 
  rval[2]=sub('H1\tH2','Entropy',rval[2])
  # use Entropy as standard columen method name
  rval[2]=sub('\tH\t','\tEntropy\t',rval[2])
  rval[2]=sub('#M','MaskLevel',rval[2])
  read.table(text=rval,header=TRUE,skip=1,comment.char="")
}
