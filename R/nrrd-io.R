#' Read nrrd file into 3d array in memory
#' 
#' @details ReadByteAsRaw=unsigned (the default) only reads unsigned byte data 
#'   as a raw array. This saves quite a bit of space and still allows data to be
#'   used for logical indexing.
#' @param file Path to a nrrd (or a connection for \code{read.nrrd.header})
#' @param origin Add a user specified origin (x,y,z) to the returned object
#' @param ReadData When FALSE just return attributes (e.g. voxel size)
#' @param AttachFullHeader Include the full nrrd header as an attribute of the 
#'   returned object (default FALSE)
#' @param ReadByteAsRaw Read 8 bit data as an R raw object rather than integer
#' @param Verbose Status messages while reading
#' @return a 3D data array with attributes compatible with gjdens objects
#' @export
read.nrrd<-function(file, origin=NULL, ReadData=TRUE, AttachFullHeader=!ReadData,
                    Verbose=FALSE, ReadByteAsRaw=c("unsigned","all","none")){
  ReadByteAsRaw=match.arg(ReadByteAsRaw)
  fc=file(file,'rb')
  h=read.nrrd.header(fc)
  # store the path because ReadNrrdHeader couldn't do it 
  # TODO more elegant way of dealing with paths when connection sent to ReadNrrdHeader
  attr(h,'path')=file
  
  # now read the data
  dataTypes=data.frame(name=I(c("int8", "uint8", "int16", "uint16", "int32", "uint32", "int64", "uint64",
                                "float", "double", "block")),
                       size=c(1,1,2,2,4,4,8,8,4,8,NA),what=I(c(rep("integer",8),rep("numeric",2),"raw")),
                       signed=c(rep(c(T,F),4),rep(T,3)))
  if(ReadByteAsRaw=="all") dataTypes$what[1:2]='raw'
  else if(ReadByteAsRaw=="unsigned") dataTypes$what[2]='raw'
  
  i=which(dataTypes$name==.standardNrrdType(h$type))
  if(length(i)!=1){
    close(fc)
    stop("Unrecognised data type")
  }
  if(!is.null(h$datafile)){
    # detached nrrd
    if(!inherits(file,"connection"))
      attr(h,'path')=file
    datafiles=NrrdDataFiles(h)
    # TODO - handle more than one datafile!
    if(length(datafiles)!=1) stop("Can currently only handle exactly one datafile")
    close(fc)
    fc=file(datafiles,open='rb')
    file=datafiles
  }
  dataLength=prod(h$sizes)
  endian=ifelse(is.null(h$endian),.Platform$endian,h$endian)
  if(Verbose) cat("dataLength =",dataLength,"dataType =",dataTypes$what[i],"size=",dataTypes$size[i],"\n")
  enc=tolower(h$encoding)
  if(ReadData){
    if(enc=="raw"){
      d=readBin(fc,what=dataTypes$what[i],n=dataLength,size=dataTypes$size[i],
                signed=dataTypes$signed[i],endian=endian)
      close(fc)
    } else if(enc%in%c("gz","gzip")) {
      # unfortunately gzcon seems to reset the connection 
      # rather than starting to read from the current location
      headerlength=seek(fc)
      close(fc)
      tf=tempfile()
      system(paste('tail -c +',sep="",headerlength+1," ",file," > ",tf))
      gzf=gzfile(tf,'rb')
      d=readBin(gzf,what=dataTypes$what[i],n=dataLength,size=dataTypes$size[i],
                signed=dataTypes$signed[i],endian=endian)
      close(gzf)
      unlink(tf)
    } else if(enc%in%c("ascii","txt","text")){
      if(dataTypes$what[i]=='integer') whatVal=integer(0) else whatVal=double(0)
      d=scan(fc,what=whatVal,nmax=dataLength,quiet=TRUE)
      close(fc)
    } else {
      stop("nrrd encoding ",enc," is not implemented")
    }
    dim(d)<-h$sizes
  } else {
    # don't read the data, we just wanted the (full) header information
    if(dataTypes$what[i]=='integer') d=integer(0) else d=double(0)
    attr(d,'datablock')=list(what=dataTypes$what[i],n=dataLength,size=dataTypes$size[i],
                             signed=dataTypes$signed[i],endian=endian)
    attr(d,'datablock')$datastartpos=seek(fc)
    close(fc)
  }
  if(AttachFullHeader) attr(d,"header")=h
  voxdims<-NrrdVoxDims(h,ReturnAbsoluteDims = FALSE)
  if(any(is.na(voxdims))){
    # missing pixel size info, so just return
    return(d)
  }
  latticeBoundingBox=rbind(c(0,0,0),(h$sizes-1)*voxdims)
  if(!missing(origin)){
    latticeBoundingBox=t(origin+t(latticeBoundingBox))
  } else if('space origin'%in%names(h)){
    # FIXME should space origin interpretation depend on node vs cell?
    # (and we are assuming that we will be working as node in R)
    latticeBoundingBox=t(h[['space origin']]+t(latticeBoundingBox))
  }
  attr(d,"BoundingBox")<-as.vector(latticeBoundingBox)
  attr(d,"x")<-seq(latticeBoundingBox[1],latticeBoundingBox[2],len=h$sizes[1])
  attr(d,"y")<-seq(latticeBoundingBox[3],latticeBoundingBox[4],len=h$sizes[2])
  attr(d,"z")<-seq(latticeBoundingBox[5],latticeBoundingBox[6],len=h$sizes[3])
  return(d)
}

#' Read the (text) header of a NRRD format file
#' 
#' @return A list with elements for the key nrrd header fields
#' @export
#' @rdname read.nrrd
read.nrrd.header<-function(file, Verbose=FALSE){
  nrrdspec=list()
  if(!inherits(file,"connection")){
    con<-file(file,open='rt')
    attr(nrrdspec,"path")=file # store file
    on.exit(close(con))
  } else con=file
  
  # Look for empty line signifying end of header
  headerLines=readLines(con,1)
  NRRDMAGIC="NRRD000"
  if(substring(headerLines,1,nchar(NRRDMAGIC))!=NRRDMAGIC)
    stop("This does not appear to be a NRRD file: ",summary(con)$description)
  nrrdkeyvals=vector('character')
  while( length(l<-readLines(con,1))>0 && l!="" ){
    headerLines=c(headerLines,l)
    if(substring(l,1,1)=="#") next
    
    if(length(grep(": ",l))>0){
      # field
      hingepos=regexpr(": ",l,fixed=TRUE)
      fieldname=substring(l,1,hingepos-1)
      # make canonical name by removing spaces if required
      if(!fieldname%in%c("space dimension","space units","space origin","space directions","measurement frame"))
        fieldname=gsub(" ","",fieldname,fixed=TRUE)
      
      fieldval=substring(l,hingepos+2,nchar(l))
      
      if(fieldname=="content"){
        # don't try and process the field values
      } else if (substring(fieldval,1,1)=="("){
        # this is a vector, first remove all spaces
        fieldval=gsub(" ","",fieldval)
        # then remove first and last brackets
        fieldval=substring(fieldval,2,nchar(fieldval)-1)
        vectorstring=unlist(strsplit(fieldval,")(",fixed=TRUE))
        tc=textConnection(vectorstring)
        fieldval=scan(tc,sep=",",quiet=TRUE)
        if(length(vectorstring)>1)
          fieldval=matrix(fieldval,byrow=TRUE,nrow=length(vectorstring))
        close(tc)
      } else if(!fieldname%in%c("type","datafile")){
        if (length(grep("^[\\-+]{0,1}[0-9.]+",fieldval,perl=T))>0) what=0
        else what=""
        tc=textConnection(fieldval)
        fieldval=scan(tc,quiet=TRUE,what=what)
        close(tc)
      } else if(fieldname=="datafile"){
        # TODO fix handling of complex datafile specifications
        # See http://teem.sourceforge.net/nrrd/format.html#detached
        if(substring(fieldval,1,4)=="LIST"){
          # we need to keep reading in lines until we hit EOF
          fieldval=c(fieldval,readLines(con))
        }
        # otherwise no special action required
      }
      nrrdspec[[fieldname]]=fieldval
      
    } else if(length(grep(":=",l))>0){
      # key val
      hingepos=regexpr(":=",l,fixed=TRUE)
      nrrdkeyvals[substring(l,1,hingepos-1)]=substring(l,hingepos+2,nchar(l))
    } else {
      warning("Skipping malformed line #",length(headerLines)," in NRRD header\n")
    }
  }
  attr(nrrdspec,'headertext')=headerLines
  attr(nrrdspec,'keyvals')=nrrdkeyvals
  nrrdspec
}

#' Check if a file is a NRRD file
#' 
#' @details Note that multiple files can be checked when a character vector of 
#'   length > 1 is provided, but only one file can be checked when a raw byte 
#'   array is provided.
#' @param f A character vector specifying the path or a raw vector with at least 8
#'   bytes.
#' @param ReturnVersion Whether to return the version of the nrrd format in
#'   which the file is encoded (1-5).
#' @param TrustSuffix Whether to trust that a file ending in .nrrd or .nhdr is a
#'   NRRD
is.nrrd<-function(f, ReturnVersion=FALSE, TrustSuffix=FALSE){
  # TrustSuffix => expect files to end in nrrd or nhdr
  if(TrustSuffix){
    if(ReturnVersion)
      stop("Cannot use return nrrd version without reading file to check nrrd magic")
    if(!is.character(f)) stop("Cannot examine suffix when filename is not provided!")
    return(grepl("\\.n(hdr|rrd)$", f, ignore.case=TRUE))
  }
  
  if(length(f)>1)
    return(sapply(f, is.nrrd, ReturnVersion=ReturnVersion))
  
  if(!file.exists(f)){
    stop("file does not exist")
  }
  
  nrrd=as.raw(c(0x4e,0x52,0x52,0x44, 0x30, 0x30, 0x30))
  magic=readBin(f, what=nrrd, n=8)
  if(any(magic[1:7]!=nrrd))
    return (FALSE)
  
  if(ReturnVersion)
    return(as.integer(magic[8])-0x30) # nb 0x30 is ASCII code for '0'
  
  TRUE
}

NrrdDataFiles<-function(nhdr,ReturnAbsPath=TRUE){
  if(!is.list(nhdr)){
    # we need to read in the nrrd header
    if(length(nhdr)>1) return(sapply(nhdr,NrrdDataFiles))
    if(!is.nrrd(nhdr)) stop("This is not a nrrd file")
    h=read.nrrd.header(nhdr)
  } else h=nhdr
  if(is.null(h$datafile)){
    # straight nrrd without detached header
    dfs=attr(h,'path')
  } else if(length(h$datafile)>1){
    # list of files
    dfs=h$datafile[-1]
    firstlineparts=unlist(strsplit(h$datafile[1],"\\s+",perl=T))
    if(length(firstlineparts)>2) stop("invalid first line of LIST datafile specifier")
    if(length(firstlineparts)==2) attr(dfs,'subdim')=as.integer(firstlineparts[2])
  } else if(grepl("%",h$datafile)){
    # Format specifier TODO
    firstlineparts=unlist(strsplit(h$datafile[1],"\\s+",perl=T))
    if(length(firstlineparts)>5 || length(firstlineparts)<4)
      stop("invalid sprintf style datafile specifier")
    rangespec=as.integer(firstlineparts[2:4])
    dfs=sprintf(firstlineparts[1],
                seq(from=rangespec[1],to=rangespec[2],by=rangespec[3]))
    if(length(firstlineparts)==5) attr(dfs,'subdim')=as.integer(firstlineparts[5])
  } else dfs=h$datafile
  
  if(ReturnAbsPath){
    # check if paths begin with /
    relpaths=substring(dfs,1,1)!="/"
    if(any(relpaths)){
      nhdrpath=attr(h,"path")
      if(is.null(nhdrpath) && ReturnAbsPath)
        stop("Unable to identify nrrd header file location to return absolute path to data files")
      dfs[relpaths]=file.path(dirname(nhdrpath),dfs[relpaths])
    }
  }
  dfs
}

.standardNrrdType<-function(type){
  if(type%in%c("float","double","block")) return (type)
  if(type%in%c("signed char", "int8", "int8_t")) return("int8")
  if(type%in%c("uchar", "unsigned char", "uint8", "uint8_t")) return("uint8")
  if(type%in%c("short", "short int", "signed short", "signed short int", "int16", "int16_t")) return("int16")
  if(type%in%c("ushort", "unsigned short", "unsigned short int", "uint16", "uint16_t")) return("uint16")
  if(type%in%c("int", "signed int", "int32", "int32_t")) return("int32")
  if(type%in%c("uint", "unsigned int", "uint32", "uint32_t")) return("uint32")
  if(type%in%c("longlong", "long long", "long long int", "signed long long", "signed long long int", "int64", "int64_t"))
    return("int64")
  if(type%in%c("ulonglong", "unsigned long long", "unsigned long long int", "uint64", "uint64_t")) return("uint64")
  return(NULL)
}
