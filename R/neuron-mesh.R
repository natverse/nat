# internal function
# read a mesh representing a neuron and return a mesh3d object
read.neuron.mesh <- function(x, ...) {
  ext=tools::file_ext(x)
  
  if(ext=="ply") {
    if(!requireNamespace('Rvcg', quietly = TRUE))
      stop("Please install suggested library Rvcg to read .ply files!")
    Rvcg::vcgPlyRead(x, ...)
  } else if(ext=="obj") {
    if(!requireNamespace('readobj', quietly = TRUE))
      stop("Please install suggested library readobj to read .obj files!")
    res=readobj::read.obj(x, convert.rgl = TRUE)
    if(length(res)>1)
      warning("Only reading 1/",length(res)," objects in: ",x)
    res[[1]]
  } else {
    stop("Unrecognised mesh file format!")
  }
}

is.ply<-function(f=NULL, bytes=NULL) {
  if(!is.null(bytes) && is.character(f) && length(f)>1)
    stop("Can only check bytes for a single file")
  tocheck=if(is.null(bytes)) f else bytes
  generic_magic_check(tocheck, "ply")
}

