#' Defines a target volume for a CMTK reformatx operation
#' 
#' @details if the character vector specifies an amiramesh file, it will be 
#'   converted to a bare \code{im3d} object and then to an appropriate 
#'   '--target-grid' specification.
#' @param target A character vector specifying an image file on disk, an
#'   \code{im3d} object (or an object that can be coerced to im3d) or a 6-or
#'   9-vector defining a grid in the form Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz].
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

cmtk.targetvolume.character <- function(target, ...) {
  if (isTRUE(substr(target,1,2) == "--")) {
    # we've already processed this, no action required
    return(target)
  }
  if (!is.nrrd(target,TrustSuffix = TRUE) &&
      isTRUE(try(is.amiramesh(target), silent = TRUE)
      )) {
    return(cmtk.targetvolume(read.im3d(target,ReadData = FALSE)))
  }
  
  shQuote(target)
}

#' @export
#' @rdname cmtk.targetvolume
cmtk.targetvolume.default <- function(target, ...) {
  # designed to catch S3 objects that do not have class list but are
  # nevertheless lists ... see cmtk.targetvolume.list for rationale
  if (is.list(target))
    return(cmtk.targetvolume(as.im3d(target)))
  # new cmtk insists that floats look like floats
  cmtkfloatvec = function(x)
    paste(sprintf("%f",x),collapse = ",")
  
  if (is.vector(target)) {
    # specify a target range c(Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz])
    if (length(target) == 9) {
      target = paste("--target-grid",
                     paste(
                       paste(target[1:3], collapse = ","),
                       cmtkfloatvec(target[4:6]),
                       cmtkfloatvec(target[7:9]),
                       sep = ":"
                     ))
    } else if (length(target) == 6) {
      target = paste("--target-grid",
                     paste(
                       paste(target[1:3], collapse = ","),
                       cmtkfloatvec(target[4:6]),
                       sep = ":"
                     ))
    } else
      stop("Incorrect target specification: ",target)
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
#' @param output The path to the output image (defaults to
#'   \code{"<targetstem>_<floatingstem>.nrrd"})
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
#' @inheritParams xformimage.cmtkreg
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
                         direction=NULL, 
                         interpolation=c("linear", "nn", "cubic", "pv", "sinc-cosine", "sinc-hamming"),
                         dryrun=FALSE, Verbose=TRUE, MakeLock=TRUE, 
                         OverWrite=c("no","update","yes"),
                         filesToIgnoreModTimes=NULL, ...){
  basestem<-function(f) tools::file_path_sans_ext(basename(as.character(f)))
  if(missing(output)){
    output=file.path(dirname(floating),paste(basestem(target),"_",basestem(floating),'.nrrd',sep=""))
  } else if(isTRUE(file.info(output)$isdir)){
    output=file.path(output,paste(basestem(target),"_",basestem(floating),'.nrrd',sep=""))
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
  
  # deal with registrations
  direction=match.arg(direction, c("forward", "inverse"), several.ok = T)
  inverseflags <- sapply(direction, function(x) ifelse(x == 'forward', '', '--inverse'))
  regspec <- paste(c(rbind(inverseflags, shQuote(path.expand(registrations)))), collapse=" ")

  cmd=cmtk.call('reformatx',if(Verbose) "--verbose" else NULL,
                outfile=shQuote(output),floating=shQuote(floating),
                mask=mask, interpolation=interpolation, ...,
                FINAL.ARGS=c(targetspec, regspec))
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
#' @details When given a label mask, returns a dataframe with a row for each 
#'   level of the label field.
#' @details Note that the Entropy column (sometimes H, sometimes Entropy) will 
#'   always be named Entropy in the returned dataframe.
#' @param f Path to image file (any CMTK compatible format)
#' @param mask Optional path to a mask file
#' @param imagetype Whether image should be treated as greyscale (default) or 
#'   label field.
#' @param masktype Whether mask should be treated as label field or binary mask 
#'   (default label)
#' @param ... Additional arguments for ctmk's statistics tool processed by 
#'   \code{\link{cmtk.call}}.
#' @inheritParams cmtk.reformatx
#' @return data.frame describing results with the following columns when image
#'   \code{f} is of \code{imagetype='greyscale'} (optionally with a mask):
#'   
#'   \itemize{
#'   
#'   \item MaskLevel (only present when using a mask) the integer value of the 
#'   label field for this region
#'   
#'   \item min The minimum voxel value within the current region
#'   
#'   \item max The maximum voxel value within the current region
#'   
#'   \item mean The mean voxel value within the current region
#'   
#'   \item sdev The standard deviation of voxel values within the current region
#'   
#'   \item n The count of \bold{all} voxel within the region (irrespective of 
#'   their value)
#'   
#'   \item Entropy Information theoretic entropy of voxel value distribution 
#'   within region
#'   
#'   \item sum Sum of voxel values within the region
#'   
#'   }
#'   
#'   When image \code{f} is of \code{imagetype='label'}, the following results
#'   are returned:
#'   
#'   \itemize{
#'   
#'   \item level The integer value of the label field for this region
#'   
#'   \item count The number of voxels in this region
#'   
#'   \item surface The surface area of this region
#'   
#'   \item volume The volume of this region
#'   
#'   \item X,Y,Z 3D coordinates of the centroid of this region
#'   
#'   }
#' @export
#' @examples
#' \dontrun{
#' cmtk.statistics('someneuron.nrrd', mask='neuropilregionmask.nrrd')
#' cmtk.statistics('somelabelfield.nrrd', imagetype='label')
#' }
cmtk.statistics<-function(f, mask, imagetype=c("greyscale","label"),
                          masktype=c("label", "binary"), ..., Verbose=FALSE){
  masktype=match.arg(masktype)
  imagetype=match.arg(imagetype)
  if(length(f)>1) return(sapply(f,cmtk.statistics,mask=mask,imagetype=imagetype,
                                masktype=masktype, ..., Verbose=Verbose))
  args=f
  if(!missing(mask)){
    args=c(ifelse(masktype=='label','--Mask','--mask'), mask, args)
  }
  if(imagetype=="label") {
    args=c("--label", args)
  }
  cmd=cmtk.call("statistics", PROCESSED.ARGS = if(Verbose) "--verbose" else NULL, 
                FINAL.ARGS = args, ... = ...)
  rval=system(cmd, intern = TRUE, ignore.stderr=!Verbose)
  if(imagetype=="label") {
    stats=gsub("[\\(\\)]","",rval)
    stats=gsub(",","\t",stats)
    # there is a final line that needs to be omitted
    stats=stats[!grepl("^Entropy:", stats)]
    # the first line is also useless
    read.table(text=stats, header=FALSE, skip=1, col.names = c("level","count","surface","volume","X","Y","Z"))
  } else {
    # there is a bug in versions of CMTK statistics <2.3.1 when used with a mask 
    # the header says that there are two entropy columns (H1,H2)
    # but in fact there is only 1. 
    rval[2]=sub('H1\tH2','Entropy',rval[2])
    # use Entropy as standard columen method name
    rval[2]=sub('\tH\t','\tEntropy\t',rval[2])
    rval[2]=sub('#M','MaskLevel',rval[2])
    read.table(text=rval,header=TRUE,skip=1,comment.char="")
  }
}
 