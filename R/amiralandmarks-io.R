# Read and Write Amira Landmarks files
# 
# @param file The file to read or write
# @param CoordinatesOnly Only return Coordinates of points
# @param Verbose Whether to write status messages
# @rdname amiralandmark-io
# @return for read.amiralandmarks a matrix for an unpaired landmark set or a
#   list of length 2 for a paired landmark set.
# @export
read.landmarks.amira<-function(file, CoordinatesOnly=TRUE, Verbose=FALSE){
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

# @param x Nx3 matrix or dataframe of landmark coordinates or a list with two
#   matrices, one for landmarks in each space.
# @rdname amiralandmark-io 
# @family amira
# @export
write.landmarks.amira<-function(x, file){
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

#' Generic functions to read/write landmarks in any supported format
#' 
#' @details Presently the supported formats are \itemize{
#'   
#'   \item Amira
#'   
#'   \item CMTK
#'   
#'   \item Fiji (see \url{http://fiji.sc/Name_Landmarks_and_Register}) }
#'   
#'   See examples section for how to produce a listing of all currently 
#'   available formats with \code{fileformats}.
#' @section Paired landmarks: Only the amiralandmarks format supports the use of
#'   paired landmarks
#' @param f Path to a file (can also be a URL)
#' @param ... Additional arguments passed on to format specific functions
#' @return for \code{read.landmarks} a matrix or list of additional class 
#'   landmarks, where the rownames specify the names of each landmark if 
#'   available.
#' @inheritParams write.neuron
#' @seealso \code{\link{fileformats}}
#' @export
read.landmarks<-function(f, ...) {
  if(grepl("^http[s]{0,1}://", f)) {
    url=f
    # download remote url to local file in tempdir
    f=file.path(tempdir(), basename(f))
    on.exit(unlink(f))
    filecontents=httr::GET(url)
    writeBin(httr::content(filecontents,type = 'raw'), con = f)
  }
  ffs=getformatreader(f, class = 'landmarks')
  if(is.null(ffs))
    stop("Unable to identify file type of:", f)
  l=match.fun(ffs$read)(f, ...)
  class(l)=c('landmarks', class(l))
  l
}

#' @rdname read.landmarks
#' @param x The landmarks object to write. Can also be a plain \code{matrix} or 
#'   \code{data.frame}.
#' @param file The path to the output file. If this does not end in an extension
#'   like \code{.landmarksAscii}, then one will be added based on the value of 
#'   the \code{ext} argument.
#' @param format Character vector specifying output format. Defaults to 
#'   \code{"amiralandmarks"}. Partial matching is used (e.g. amira is
#'   sufficient).
#' @param ext Optional character vector specifying a new or non-standard 
#'   extension to use for output file, including the period (e.g. 
#'   \code{ext='.am'}). When \code{ext=NULL}, the default, the default extension
#'   for the selected \code{format} will be added if \code{f} does not have an 
#'   extension. When \code{ext=NA}, the extension will not be modified and no 
#'   extension will be appended if \code{f} does not have one.
#' @return For \code{write.landmarks} the path to the written file, invisibly.
#' @export
#' @examples
#' ## Listing of supported fileformats for landmarks
#' fileformats(class = 'landmarks', rval = "info")
#' 
#' ## round trip tests
#' m=matrix(rnorm(6), ncol=3)
#' rownames(m)=c("nose", "ear")
#' f=write.landmarks(m, file='knee', format='cmtk')
#' read.landmarks(f)
#' 
#' # write in amira format which does not support named landmarks
#' f2=write.landmarks(m, file='knee', format='amira')
#' read.landmarks(f2)
#' 
#' # clean up
#' unlink(c(f,f2))
write.landmarks<-function(x, file, format='amiralandmarks', ext=NULL, Force=FALSE,
                          MakeDir=TRUE, ...) {
 fw=getformatwriter(format=format, file=file, ext=ext, class='landmarks')
  file=fw$file
  if(!Force && file.exists(file)){
    warning(file," already exists; use Force=T to overwrite")
    return(NA_character_)
  }
  if(!file.exists(dirname(file))){
    if(MakeDir){
      if(!dir.create(dirname(file)))
        stop("Unable to create ",dirname(file))
    } else {
      stop(dirname(file)," does not exist; use MakeDir=T to overwrite")
    }
  }
  if(!file.access(file, mode=2)){
    stop("Unable to write to file ",file)
  }
  
  # OK all fine, so let's write
  match.fun(fw$write)(x, file=file, ...)
  invisible(file)
}
