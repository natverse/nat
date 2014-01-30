#' Read and Write Amira Landmarks files
#' 
#' @param file The file to read or write
#' @param CoordinatesOnly Only return Coordinates of points
#' @param Verbose Whether to write status messages
#' @rdname amiralandmark-io
#' @return for read.amiralandmarks a matrix for an unpaired landmark set or a
#'   list of length 2 for a paired landmark set.
#' @export
read.amiralandmarks<-function(file, CoordinatesOnly=TRUE, Verbose=FALSE){
  r=read.amiramesh(file, header=TRUE, simplify=FALSE, Verbose=Verbose)
  headerLines=attr(r,"header")
  dataDef=attr(r,"dataDef")
  
  # get the number of landmark sets
  NumSetLine=grep("NumSets\\s+[0-9]{1,}",headerLines,value=TRUE)
  if(length(NumSetLine)==0) stop(paste("Unable to establish number of amira landmarks sets in",file))
  nSets=as.numeric(sub(".*NumSets\\s+([0-9]{1,}).*","\\1",NumSetLine))
  
  # get the number of data sections
  nDataSections=nrow(dataDef)
  nSectionsPerSet=nDataSections/nSets
  if(round(nSectionsPerSet)!=nSectionsPerSet) stop(paste("Unable to parse amira landmarks sets",file,":too many data sections!"))
  
  if(CoordinatesOnly){
    chosenSections=seq(from=1,by=nSectionsPerSet,length=nSets)
    r=r[chosenSections]
  }
  if(length(r)==1) r[[1]]
  else r
}

#' @param x Nx3 matrix or dataframe of landmark coordinates or a list with two
#'   matrices, one for landmarks in each space.
#' @rdname amiralandmark-io 
#' @family amira
#' @export
write.amiralandmarks<-function(x, file){
  if(is.list(x) && !is.data.frame(x)) l=x
  else l=list(x)
  nSets=length(l)
  nummarkers=sapply(l,nrow)
  if(length(unique(nummarkers))!=1) stop("Must have just an equal number of markers in paired landmark sets")
  nummarkers=nummarkers[1]
  cat("# AmiraMesh 3D ASCII 2.0\n\ndefine Markers",nummarkers,"\n\nParameters {\n\tContentType \"LandmarkSet\",\n\tNumSets",nSets,"\n}\n",file=file)
  for(i in 1:nSets){
    coordsuffix=ifelse(i==1,"",i)
    cat("Markers { float[3] Coordinates",coordsuffix," } @",i,sep="","\n",file=file,append=T)
  }
  for(i in 1:nSets){
    cat("@",i,sep="","\n",file=file,append=T)
    write.table(l[[i]],col.names=F,row.names=F,file=file,append=TRUE)
    cat("\n",file=file,append=T)
  }
}
