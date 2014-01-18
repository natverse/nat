# wrappers for some CMTK command line tools

#' Convert CMTK registration to homogeneous affine matrix with dof2mat
#' 
#' @details Transpose is true by default since this results in the orientation
#'   of cmtk output files matching the orientation in R. Do not change this
#'   unless you're sure you know what you're doing!
#' @param reg Path to input registration file or 5x3 matrix of CMTK parameters.
#' @param Transpose ouput matrix so that form on disk matches R's convention.
#' @param version Whether to return CMTK version string
#' @return 4x4 transformation matrix
#' @family cmtk-commandline
#' @family cmtk-geometry
#' @export
cmtk.dof2mat<-function(reg, Transpose=TRUE, version=FALSE){
  dof2mat=file.path(cmtk.bindir(check=TRUE),"dof2mat")
  if(version) return(system2(dof2mat,'--version',stdout=TRUE))
  
  if(is.numeric(reg)){
    params<-reg
    reg<-tempfile(fileext='.list')
    on.exit(unlink(reg,recursive=TRUE))
    write.cmtkreg(params,foldername=reg)
  }
  
  cmd=paste(dof2mat,ifelse(Transpose,'--transpose',''),shQuote(path.expand(reg)))
  rval=system(cmd,intern=TRUE)
  numbers=as.numeric(unlist(strsplit(rval,"\t")))
  matrix(numbers,ncol=4,byrow=TRUE)
}

#' Use CMTK mat2dof to convert homogeneous affine matrix into CMTK registration
#' 
#' @details If no output file is supplied, 5x3 params matrix will be returned 
#'   directly. Otherwise a logical will be returned indicating success or 
#'   failure at writing to disk.
#' @details Transpose is true by default since this results in an R matrix with 
#'   the transpose in the fourth column being correctly interpreted by cmtk.
#' @param m Homogenous affine matrix (4x4) last row 0 0 0 1 etc
#' @param f Output file (optional)
#' @param centre Centre for rotation (optional 3-vector)
#' @param Transpose the input matrix so that it is read in as it appears on disk
#' @param version When TRUE, function returns CMTK version number of mat2dof
#'   tool
#' @return 5x3 matrix of CMTK registration parameters or logical
#' @family cmtk-commandline
#' @family cmtk-geometry
#' @export
cmtk.mat2dof<-function(m, f=NULL, centre=NULL, Transpose=TRUE, version=FALSE){
  mat2dof=file.path(cmtk.bindir(check=TRUE),'mat2dof')
  if(version) return(system2(mat2dof,'--version',stdout=TRUE))
  if(!is.matrix(m) || nrow(m)!=4 || ncol(m)!=4) stop("Please give me a homogeneous affine matrix (4x4)")
  inf=tempfile()
  on.exit(unlink(inf),add=TRUE)
  
  write.table(m, file=inf, sep='\t', row.names=F, col.names=F)
  # always transpose because mat2dof appears to read the matrix with last column being 0 0 0 1
  
  cmd=if(Transpose) paste(mat2dof,'--transpose') else mat2dof
  if(!is.null(centre)) {
    if(length(centre)!=3) stop("Must supply 3-vector for centre")
    cmd=paste(cmd,'--center',paste(centre, collapse=","))
  }
  if(is.null(f)){
    cmd=paste(cmd,sep="<",shQuote(inf))
    params=read.table(text=system(cmd,intern=T),sep='\t',comment.char="")[,2]
    if(length(params)!=15) stop("Trouble reading mat2dof response")
    return(matrix(params,ncol=3,byrow=TRUE))
  } else {
    cmd=paste(cmd,'--list',shQuote(path.expand(f)),"<",shQuote(inf))
    return(system(cmd)==0)
  }
}

#' Return path to directory containing CMTK binaries
#' 
#' @details Queries options('nat.cmtk.bindir') if \code{firtsdir} is not 
#'   specified. If that does not contain the appropriate binaries, it will look 
#'   in the system PATH and then a succession of plausible places until it finds
#'   something. Setting \code{options(nat.cmtk.bindir=NA)} or passing 
#'   \code{firstdir=NA} will stop the function from trying to locate CMTK, 
#'   always returning NULL unless check=TRUE when it will error out.
#' @param firstdir Character vector specifying path containing CMTK binaries or 
#'   NA (see details). This defaults to options('nat.cmtk.bindir').
#' @param extradirs Where to look if CMTK is not in \code{firstdir} or the PATH
#' @param set Whether to set options('nat.cmtk.bindir') with the found directory
#' @param check Whether to (re)check that a path that has been set appropriately
#'   in options(nat.cmtk.bindir='/some/path') or now found in the PATH or 
#'   alternative directories. Will throw an error on failure.
#' @param cmtktool Name of a specific cmtk tool which will be used to identify 
#'   the location of all cmtk binaries.
#' @return Character vector giving path to CMTK binary directory or NULL when 
#'   this cannot be found.
#' @export
#' @aliases cmtk
#' @section Installation: It is recommended to install released CMTK versions 
#'   available from the \href{www.nitrc.org/projects/cmtk/}{NITRC website}. A 
#'   bug in composition of affine transformations from CMTK parameters in the 
#'   CMTK versions <2.4 series means that CMTK>=3.0 is strongly recommended. 
#'   CMTK v3 registrations are not backwards compatible with CMTK v2, but CMTKv3
#'   can correctly interpret and convert registrations from earlier versions.
#' @examples
#' cmtk.bindir()
#' \dontrun{
#' # set options('nat.cmtk.bindir') according to where cmtk was found
#' op=options(nat.cmtk.bindir=NULL)
#' cmtk.bindir(set=TRUE)
#' options(op)
#' }
cmtk.bindir<-function(firstdir=getOption('nat.cmtk.bindir'),
                      extradirs=c('~/bin','/usr/local/lib/cmtk/bin',
                                  '/usr/local/bin','/opt/local/bin',
                                  '/Applications/IGSRegistrationTools/bin'),
                      set=FALSE, check=FALSE, cmtktool='gregxform'){
  bindir=NULL
  if(!is.null(firstdir)) {
    bindir=firstdir
    if(check && !file.exists(file.path(bindir,cmtktool)))
      stop("cmtk is _not_ installed at:", bindir,
           "\nPlease check value of options('nat.cmtk.bindir')")
  }
  if(is.null(bindir)){
    ow=options(warn=-1)
    cmtktool_exists=system(paste('which',cmtktool),ignore.stdout=TRUE,
                           ignore.stderr=TRUE)==0
    options(ow)
    if(cmtktool_exists){
      # identify location of chosen cmtk tool on current on path
      cmtktoolpath<-system(paste('which',cmtktool),intern=TRUE,ignore.stderr=TRUE)
      bindir=dirname(cmtktoolpath)
    } else {
      # check some plausible locations
      for(d in extradirs){
        if(file.exists(file.path(d,cmtktool))) {
          bindir=d
          break
        }
      }
    }
  }
  if(!is.null(bindir)){
    if(is.na(bindir)) bindir=NULL
    else bindir=path.expand(bindir)
  }
  if(check && is.null(bindir))
    stop("Cannot find CMTK. Please install from ",
         "http://www.nitrc.org/projects/cmtk and make sure that it is your path!")
  
  if(set)
    options(nat.cmtk.bindir=bindir)
  bindir
}
