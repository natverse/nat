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
#' @param ReadByteAsRaw Either a character vector or a logical vector specifying
#'   when R should read 8 bit data as an R \code{raw} vector rather than 
#'   \code{integer} vector.
#' @param Verbose Status messages while reading
#' @return a 3D data array with attributes compatible with gjdens objects
#' @export
read.nrrd<-function(file, origin=NULL, ReadData=TRUE, AttachFullHeader=!ReadData,
                    Verbose=FALSE, ReadByteAsRaw=c("unsigned","all","none")){
  if(is.logical(ReadByteAsRaw))
    ReadByteAsRaw=ifelse(ReadByteAsRaw, 'all', 'none')
  else ReadByteAsRaw=match.arg(ReadByteAsRaw, c("unsigned","all","none"))
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
    datafiles=nrrd.datafiles(h)
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
  on.exit(close(fc))
  if(ReadData){
    if(enc%in%c("gz","gzip")) fc=gzcon(fc)
    if(enc%in%c("gz","gzip",'raw')){
      d=readBin(fc,what=dataTypes$what[i],n=dataLength,size=dataTypes$size[i],
                 signed=dataTypes$signed[i],endian=endian)
    } else if(enc%in%c("ascii","txt","text")){
      if(dataTypes$what[i]=='integer') whatVal=integer(0) else whatVal=double(0)
      d=scan(fc,what=whatVal,nmax=dataLength,quiet=TRUE)
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
  }
  if(AttachFullHeader) attr(d,"header")=h
  voxdims<-nrrd.voxdims(h,ReturnAbsoluteDims = FALSE)
  if(any(is.na(voxdims))){
    # missing pixel size info, so just return
    return(d)
  }
  im3d(d, dims=h$sizes, voxdims=voxdims, origin=h[['space origin']])
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
#' @param f A character vector specifying the path or a raw vector with at least
#'   8 bytes.
#' @param bytes optional raw vector of at least 8 bytes from the start of a
#'   single file (used in preference to reading file \code{f}).
#' @param ReturnVersion Whether to return the version of the nrrd format in 
#'   which the file is encoded (1-5).
#' @param TrustSuffix Whether to trust that a file ending in .nrrd or .nhdr is a
#'   NRRD
#' @export
is.nrrd<-function(f=NULL, bytes=NULL, ReturnVersion=FALSE, TrustSuffix=FALSE){
  if(is.raw(f)) {
    bytes=f
    f=NULL
  }
  # TrustSuffix => expect files to end in nrrd or nhdr
  if(TrustSuffix){
    if(ReturnVersion)
      stop("Cannot use return nrrd version without reading file to check nrrd magic")
    if(!is.character(f)) stop("Cannot examine suffix when filename is not provided!")
    return(grepl("\\.n(hdr|rrd)$", f, ignore.case=TRUE))
  }
  
  if(!is.null(f)){
    if(length(f)>1)
      return(sapply(f, is.nrrd, ReturnVersion=ReturnVersion))
    if(!file.exists(f))
      stop("file does not exist")
  }
  
  nrrd=as.raw(c(0x4e,0x52,0x52,0x44, 0x30, 0x30, 0x30))
  magic=if(!is.null(bytes)) bytes else readBin(f, what=nrrd, n=8)
  if(any(magic[1:7]!=nrrd))
    return (FALSE)
  
  if(ReturnVersion)
    return(as.integer(magic[8])-0x30) # nb 0x30 is ASCII code for '0'
  
  TRUE
}

nrrd.datafiles<-function(nhdr,ReturnAbsPath=TRUE){
  if(!is.list(nhdr)){
    # we need to read in the nrrd header
    if(length(nhdr)>1) return(sapply(nhdr,nrrd.datafiles))
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

#' Return voxel dimensions (by default absolute voxel dimensions)
#' 
#' @details NB Can handle off diagonal terms in space directions matrix, BUT
#'   assumes that space direction vectors are orthogonal.
#' @param file path to nrrd/nhdr file or a list containing a nrrd header
#' @param ReturnAbsoluteDims Defaults to returning absolute value of dims even 
#'   if there are any negative space directions
#' @return voxel dimensions as numeric vector
#' @author jefferis
#' @seealso \code{\link{read.nrrd.header}}
#' @export
nrrd.voxdims<-function(file, ReturnAbsoluteDims=TRUE){
  if(is.character(file))
    h=read.nrrd.header(file)
  else
    h=file
  if('space directions'%in%names(h)){
    voxdims=rowSums(sqrt(h[['space directions']]^2))
  } else if ('spacings'%in%names(h)){
    voxdims=h[["spacings"]]
  } else {
    warning("Unable to find voxel dimensions in nrrd: ",file)
    voxdims=rep(NA,h$dimension)
  }
  
  # Sometimes get -ve space dirs, take abs if requested
  if(ReturnAbsoluteDims) abs(voxdims)
  else voxdims
}

#' Write a 3d array to a NRRD file
#' 
#' Produces a lattice format file i.e. one with a regular x,y,z grid
#' @param x A 3d data array
#' @param file Character string naming a file
#' @param enc One of three supported nrrd encodings ("gzip", "raw", "text")
#' @param dtype The data type to write. One of "float","byte", "short", 
#'   "ushort", "int", "double"
#' @param endian One of "big" or "little". Defaults to \code{.Platform$endian}.
#' @export
#' @seealso \code{\link{read.nrrd}, \link{.Platform}}
write.nrrd<-function(x, file, enc=c("gzip","raw","text"),
                     dtype=c("float","byte", "short", "ushort", "int", "double"),
                     endian=.Platform$endian){
  enc=match.arg(enc)
  endian=match.arg(endian, c('big','little'))
  dtype=match.arg(dtype)

  nrrdDataTypes=structure(c("uint8","uint16","int16","int32","float","double"),
                          names=c("byte", "ushort", "short", "int", "float", "double"))
  
  nrrdDataType=nrrdDataTypes[dtype]
  if(is.na(nrrdDataType))
    stop("Unable to write nrrd file for data type: ",dtype)
  
  cat("NRRD0004\n", file=file)
  cat("encoding: ", enc,"\ntype: ", nrrdDataType, "\n",sep="", append=TRUE, 
      file=file)
  cat("dimension: ", length(dim(x)), "\nsizes: ", paste(dim(x), collapse=" "),
      "\n",sep="", append=TRUE, file=file)
  voxdims=voxdims(x)
  if(length(voxdims) && !(any(is.na(voxdims)))) {
    origin=attr(x,'origin')
    if(length(origin)){
      # we need to write out as space origin + space directions
      nrrdvec=function(x) sprintf("(%s)",paste(x,collapse=","))
      cat("space dimension:", length(dim(x)), "\n", file=file, append=TRUE)
      cat("space origin:", nrrdvec(origin),"\n", file=file, append=TRUE)
      cat("space directions:",
          nrrdvec(c(voxdims[1], 0, 0)),
          nrrdvec(c(0, voxdims[2], 0)),
          nrrdvec(c(0, 0, voxdims[3])),
          '\n', file=file, append=TRUE)
    } else {
      cat("spacings:", voxdims,"\n", file=file, append=TRUE)
    }
  }
  
  if(!is.list(x)) d=x else d=x$estimate
  
  # Find data type and size for amira
  dtype=match.arg(dtype)	
  dtypesize<-c(4,1,2,2,4,8)[which(dtype==c("float","byte", "short","ushort", 
                                           "int", "double"))]
  # Set the data mode which will be used in the as.vector call at the
  # moment that the binary data is written out.
  if(dtype%in%c("byte","short","ushort","int")) dmode="integer"
  if(dtype%in%c("float","double")) dmode="numeric"
  # record byte ordering if necessary
  if(enc!='text' && dtypesize>1)
    cat("endian: ", endian,"\n", sep="", file=file, append=TRUE)
  # Single blank line terminates header
  cat("\n", file=file, append=TRUE)
  
  if(enc=='text'){
    write(as.vector(d,mode=dmode),ncolumns=1,file=file,append=TRUE)
  } else {
    if(enc=="gzip") fc=gzfile(file,"ab")
    else fc=file(file,open="ab") # ie append, bin mode
    writeBin(as.vector(d, mode=dmode), fc, size=dtypesize, endian=endian)
    close(fc)
  }
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
