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
