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
