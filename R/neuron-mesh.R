# internal function
# read a mesh representing a neuron and return a mesh3d object
read.neuron.mesh <- function(x, updateNormals=FALSE, clean=FALSE, ...) {
  ext=tools::file_ext(x)
  
  if(ext=="ply") {
    if(!requireNamespace('Rvcg', quietly = TRUE))
      stop("Please install suggested library Rvcg to read .ply files!")
    m=Rvcg::vcgPlyRead(x, updateNormals=updateNormals, clean=clean, ...)
    if(!inherits(m, 'shape3d'))
      class(m)=union(class(m), 'shape3d')
    m
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

write.neuron.ply <- function(x, file, binary=TRUE, ...) {
  write.neuron.mesh(x, file=file, format="ply", binary=binary, ...)
}

write.neuron.obj <- function(x, file, ... ) {
  write.neuron.mesh(x, file=file, format="obj", ...)
}

write.neuron.mesh <- function(x, file, format=c("ply", "obj"), ...) {
  if(!requireNamespace('Rvcg', quietly = TRUE))
    stop("Please install suggested library Rvcg to write .",format," files!")
  if(!inherits(x, 'mesh3d')) {
    x=tryCatch(as.mesh3d(x), error=function(e) stop("Unable to convert x to mesh3d object! Only neuron meshes can be written in ",format," format!"))
  }
  if(format=="ply") {
    if(!isTRUE(tools::file_ext(file)=="ply")) {
      # ply format must end in .ply 
      origfile=file
      file=tempfile(fileext = '.ply')
      on.exit(file.copy(file, origfile, overwrite = T), add = T)
      on.exit(unlink(file), add = T)
    }
    Rvcg::vcgPlyWrite(x, filename=file, writeCol = F, writeNormals = F, ...)
  }
  else if(format=="obj")
    Rvcg::vcgObjWrite(x, filename=file, writeNormals=F, ...)
  else stop("Unknown format")
}

read.neuron.ngmesh <- function(x, format=c('mesh3d', "raw"), ...) {
  format=match.arg(format)
  bytes <- if(inherits(x, 'response')) {
    httr::content(x, as = 'raw')
  } else if(is.list(x) && !is.null(x$content) && is.raw(x$content)) {
    x$content
  } else if(is.character(x)) {
    if(grepl("^http[s]{0,1}://", x)) {
    res=httr::GET(x, ...)
    httr::stop_for_status(res)
    httr::content(res, as = 'raw')
    } else readBin(x, what=raw(), n=file.size(x))
  } else stop("Invalid input. I can accept an httr or curl response, a file or url!")

  decode_neuroglancer_mesh(bytes, format = format)
}

decode_neuroglancer_mesh <- function(bytes, format=c('mesh3d', "raw")) {
  format=match.arg(format)
  con=rawConnection(bytes)
  on.exit(close(con))
  
  nverts=readBin(con, what = 'int', size = 4, n=1)
  verts=readBin(con, what='numeric', n=nverts*3, size=4)
  nidxs=length(bytes)/4-1L-length(verts)
  idx=readBin(con, what='int', n=nidxs, size=4)
  
  if(format=='raw') {
    structure(list(
      v = matrix(verts, ncol = 3, byrow = T),
      i = matrix(idx, ncol = 3, byrow = T)
    ),
    class = 'ngmesh')
  } else{
    rgl::tmesh3d(
      matrix(verts, nrow=3, byrow = F),
      matrix(idx+1L, nrow=3, byrow = F),
      homogeneous = F)
  }
}

write.neuron.ngmesh <- function(x, file, ...) {
  
  if(!inherits(x, 'mesh3d')) {
    x=tryCatch(as.mesh3d(x), error=function(e) stop("Unable to convert x to mesh3d object! Only neuron meshes can be written in ",format," format!"))
  }
  
  nverts=as.integer(ncol(x$vb))
  stopifnot(isTRUE(nverts>0))
  verts <- c(x$vb[1:3, , drop = FALSE])
  mode(verts)='double'
  idxs=c(x$it-1L)
  mode(idxs)='integer'
  if(is.character(file)) {
    file=file(file, open = 'wb')
    on.exit(close(file))
  }
  writeBin(nverts, file, size = 4, endian='little')
  writeBin(verts, file, size=4, endian='little')
  writeBin(idxs, file, size=4, endian='little')
}

