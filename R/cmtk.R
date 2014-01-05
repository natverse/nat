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
#' @export
cmtk.dof2mat<-function(reg, Transpose=TRUE, version=FALSE){
  cmd="dof2mat"
  if(version) return(system2(cmd,'--version',stdout=TRUE))
  if(Transpose) cmd=paste(cmd,'--transpose')
  
  if(is.numeric(reg)){
    params<-reg
    reg<-tempfile(fileext='.list')
    on.exit(unlink(reg,recursive=TRUE))
    write.cmtkreg(params,foldername=reg)
  }
  
  cmd=paste(cmd,shQuote(reg))
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
#' @export
cmtk.mat2dof<-function(m, f=NULL, centre=NULL, Transpose=TRUE, version=FALSE){
  cmd="mat2dof"
  if(version) return(system2(cmd,'--version',stdout=TRUE))
  if(!is.matrix(m) || nrow(m)!=4 || ncol(m)!=4) stop("Please give me a homogeneous affine matrix (4x4)")
  inf=tempfile()
  on.exit(unlink(inf),add=TRUE)
  
  write.table(m, file=inf, sep='\t', row.names=F, col.names=F)
  # always transpose because mat2dof appears to read the matrix with last column being 0 0 0 1
  
  if(Transpose) cmd=paste(cmd,'--transpose')
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
    cmd=paste(cmd,'--list',shQuote(f),"<",shQuote(inf))
    return(system(cmd)==0)
  }
}
