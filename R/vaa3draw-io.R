# Routines for reading/writing Hanchuan Peng's raw image format usedin Vaa3D

#' Check if a file is in the raw image format used by Hanchuan Peng's Vaa3D
#' 
#' @description See http://www.vaa3d.org/
#' https://svn.janelia.org/penglab/projects/vaa3d/trunk/imagej_io/v3draw_io_imagej/raw_reader.java
#' @details Note that multiple files can be checked when a character vector of 
#'   length > 1 is provided, but only one file can be checked when a raw byte 
#'   array is provided.
#' @param f A character vector specifying the path or a raw vector with at least
#'   24 bytes.
#' @export
is.vaa3draw<-function(f){
  if(!is.raw(f)){
    if(length(f)>1)
      return(sapply(f, is.vaa3draw))
    
    if(!file.exists(f)){
      stop("file does not exist")
    }
  }
  
  # dput(charToRaw("raw_image_stack_by_hpeng"))
  raw_image_stack_by_hpeng=as.raw(c(0x72, 0x61, 0x77, 0x5f, 0x69, 0x6d, 0x61, 
                                    0x67, 0x65, 0x5f, 0x73, 0x74, 0x61, 0x63,
                                    0x6b, 0x5f, 0x62, 0x79, 0x5f, 0x68, 0x70,
                                    0x65, 0x6e, 0x67))
  magic=readBin(f, what=raw_image_stack_by_hpeng, 
                n=length(raw_image_stack_by_hpeng))
  
  isTRUE(all(magic==raw_image_stack_by_hpeng))
}

# Read vaa3d raw images into im3d objects, optionally subsetting input array
# @examples
# \dontrun{
# read.vaa3draw.im3d("L1DS1_crop_straight.raw",ReadData = F,chan=2)
# }
read.vaa3draw.im3d<-function(f, ReadData=TRUE, ..., chan=NA){
  x=read.vaa3draw(f=f, ReadData = ReadData, ...)
  dims=attr(x,'header')$sizes
  dims=dims[dims>1]
  if(is.na(chan)){
    if(length(dims)>3) stop("im3d is restricted to 3D image data")
  } else {
    if(ReadData)
      x=x[,,,chan]
    dims=dims[1:3]
  }
  im3d(x, dims)
}

#' Read Vaa3d format image data
#' 
#' @param f Path to image to read
#' @param ReadData Whether to read in data or just parse header
#' @param Verbose Whether to print status messages
#' @param ReadByteAsRaw Can reduce memory footprint by reading 8 bit data as a 
#'   raw rather than 4 byte interegers.
#' @export
read.vaa3draw<-function(f, ReadData=TRUE, Verbose=FALSE, ReadByteAsRaw=FALSE){
  # datatype has 2 bytes, and sz has 4*4 bytes and endian flag has 1 byte.
  fc=file(f,'rb')
  filesize=file.info(f)$size
  on.exit(close(fc))
  
  nh=list(encoding='raw')
  
  headerLength.short=24+1+2+4*2
  headerLength.long=24+1+2+4*4
  header=readBin(fc, what='raw', n=headerLength.short)
  stopifnot(is.vaa3draw(header))
  nh$endian=switch(readBin(header[25],what='character'), B='big', L='little', stop("Unknown endian-ness"))

  if(Verbose) message(nh$endian, ' endian')

  dataTypeSize=readBin(header[26:27],what=integer(),n=4,size=2,endian=nh$endian)
  if(dataTypeSize==1){
    datamode=if(ReadByteAsRaw) "raw" else "integer"
    nh$type='uint8'
  } else if(dataTypeSize==2){
    datamode="integer"
    nh$type='uint16'
  } else if(dataTypeSize==4){
    datamode="numeric"
    nh$type='float'
  } else{
    stop("Unknown datatype.")
  }
  if(Verbose) message('dataTypeSize: ', dataTypeSize)
  
  # dims could be specified as block of 4 int32s or int64s
  nh$sizes=readBin(header[28:headerLength.short],what=integer(),n=4,size=2,endian=nh$endian)
  nh$byteskip=headerLength.short
  if((prod(nh$sizes)+headerLength.short) != filesize){
    if(Verbose) message("Checking if image dims are stored as int64")
    # try again assuming dims are in 4 byte integers
    # read in another 8 bytes (i.e. 4)
    header=c(header, readBin(fc, what='raw', n=8))
    nh$sizes=readBin(header[28:headerLength.long],what=integer(),n=4,size=4,endian=nh$endian)
    if((prod(nh$sizes)+headerLength.long) != filesize) stop("image dimensions do not match")
    nh$byteskip=headerLength.long
    
  }
  nh$dimension=4
  if(Verbose) message("Image dims are: ", paste(nh$sizes,collapse=' x '))
  nh$datafile=f
  
  dims=nh$sizes[nh$sizes>1]
  if(ReadData){
    dens=readBin(fc,what=datamode,n=prod(nh$sizes),size=dataTypeSize,endian=nh$endian)
    # Keep only dimensions with more than 1 voxel.
    dim(dens)<-dims
    attr(dens,'header')=nh
    dens
  } else {
    structure(vector(mode=datamode), header=nh)
  }
}
