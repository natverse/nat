#' Read AmiraMesh data in binary or ascii format
#' 
#' @details reading byte data as raw arrays requires 1/4 memory but complicates
#'   arithmetic.
#' @param file Name of file (or connection) to read
#' @param sections character vector containing names of sections
#' @param header Whether to include the full unprocessesd text header as an 
#'   attribute of the returned list.
#' @param simplify If there is only one datablock in file do not return wrapped 
#'   in a list (default TRUE).
#' @param endian Whether multibyte data types should be treated as big or little
#'   endian. Default of NULL checks file or uses \code{.Platform$endian}
#' @param ReadByteAsRaw Logical specifying whether to read 8 bit data as an R 
#'   \code{raw} vector rather than \code{integer} vector (default: FALSE).
#' @param Verbose Print status messages
#' @return list of named data chunks
#' @importFrom nat.utils is.gzip
#' @rdname amiramesh-io
#' @export
#' @seealso \code{\link{readBin}, \link{.Platform}}
#' @family amira
read.amiramesh<-function(file,sections=NULL,header=FALSE,simplify=TRUE,
                         endian=NULL,ReadByteAsRaw=FALSE,Verbose=FALSE){
  firstLine=readLines(file,n=1)
  if(!any(grep("#\\s+(amira|hyper)mesh",firstLine,ignore.case=TRUE))){
    warning(paste(file,"does not appear to be an AmiraMesh file"))
    return(NULL)
  }
  binaryfile="binary"==tolower(sub(".*(ascii|binary).*","\\1",firstLine,ignore.case=TRUE))
  
  # Check if file is gzipped
  con=if(is.gzip(file)) gzfile(file) else file(file)
  open(con, open=ifelse(binaryfile, 'rb', 'rt'))
  on.exit(try(close(con),silent=TRUE))
  h=read.amiramesh.header(con,Verbose=Verbose)
  parsedHeader=h[["dataDef"]]
  if(is.null(endian) && is.character(parsedHeader$endian)) {
    endian=parsedHeader$endian[1]
  }
  if(ReadByteAsRaw){
    parsedHeader$RType[parsedHeader$SimpleType=='byte']='raw'
  }
  if(is.null(sections)) sections=parsedHeader$DataName
  else sections=intersect(parsedHeader$DataName,sections)
  if(length(sections)){
    if(binaryfile){
      filedata=.read.amiramesh.bin(con,parsedHeader,sections,Verbose=Verbose,endian=endian)
      close(con)
    } else {
      close(con)
      filedata=read.amiramesh.ascii(file,parsedHeader,sections,Verbose=Verbose)
    }
  } else {
    # we don't have any data to read - just make a dummy return object to which
    # we can add attributes
    filedata<-switch(parsedHeader$RType[1],
                     integer=integer(0), raw=raw(), numeric(0))
  }
  
  if(!header) h=h[setdiff(names(h),c("header"))]	
  for (n in names(h))
    attr(filedata,n)=h[[n]]
  
  # unlist?
  if(simplify && is.list(filedata) && length(filedata)==1){
    filedata2=filedata[[1]]
    attributes(filedata2)=attributes(filedata)
    dim(filedata2)=dim(filedata[[1]])
    filedata=filedata2
  } 
  return(filedata)
}

.read.amiramesh.bin<-function(con, df, sections, endian=endian, Verbose=FALSE){
  l=list()
  for(i in seq(len=nrow(df))){
    if(Verbose) cat("Current offset is",seek(con),";",df$nBytes[i],"to read\n")
    
    if(all(sections!=df$DataName[i])){
      # Just skip this section
      if(Verbose) cat("Skipping data section",df$DataName[i],"\n")
      seek(con,df$nBytes[i],origin="current")
    } else {
      if(Verbose) cat("Reading data section",df$DataName[i],"\n")
      if(df$HxType[i]=="HxByteRLE"){
        d=readBin(con,what=raw(0),n=as.integer(df$HxLength[i]),size=1)
        d=decode.rle(d,df$SimpleDataLength[i])
        x=as.integer(d)
      } else {
        if(df$HxType[i]=="HxZip"){
          uncompressed=read.zlib(con, compressedLength=as.integer(df$HxLength[i]))
        } else {
          uncompressed=con
        }
        whatval=switch(df$RType[i], integer=integer(0), raw=raw(0), numeric(0))
        x=readBin(uncompressed,df$SimpleDataLength[i],size=df$Size[i],
                  what=whatval,signed=df$Signed[i],endian=endian)
      }
      # note that first dim is moving fastest
      dims=unlist(df$Dims[i])
      # if the individual elements have subelements
      # then put those as innermost (fastest) dim
      if(df$SubLength[i]>1) dims=c(df$SubLength[i],dims)
      ndims=length(dims)
      if(ndims>1) dim(x)=dims
      if(ndims==2) x=t(x) # this feels like a hack, but ...
      l[[df$DataName[i]]]=x
    }
    if(df$SimpleDataLength[i]){
      # Skip return at end of section iff we had some data to read
      readLines(con,n=1)
      nextSectionHeader=readLines(con,n=1)
      if(Verbose) cat("nextSectionHeader = ",nextSectionHeader,"\n")
    }
  }
  l
}

# Read ASCII AmiraMesh data  
# @details Does not assume anything about line spacing between sections
# @param df dataframe containing details of data in file
read.amiramesh.ascii<-function(file, df, sections, Verbose=FALSE){
  l=list()
  #  df=subset(df,DataName%in%sections)
  df=df[order(df$DataPos),]
  if(inherits(file,'connection')) 
    con=file
  else {
    # rt is essential to ensure that readLines behaves with gzipped files
    con=file(file,open='rt')
    on.exit(close(con))
  }
  readLines(con, df$LineOffsets[1]-1)
  for(i in seq(len=nrow(df))){
    if(df$DataLength[i]>0){
      # read some lines until we get to a data section
      nskip=0
      while( substring(readLines(con,1),1,1)!="@"){nskip=nskip+1}
      if(Verbose) cat("Skipped",nskip,"lines to reach next data section")
      if(Verbose) cat("Reading ",df$DataLength[i],"lines in file",file,"\n")
      
      if(df$RType[i]=="integer") whatval=integer(0) else whatval=numeric(0)
      datachunk=scan(con,what=whatval,n=df$SimpleDataLength[i],quiet=!Verbose)
      # store data if required
      if(df$DataName[i]%in%sections){
        # convert to matrix if required
        if(df$SubLength[i]>1){
          datachunk=matrix(datachunk,ncol=df$SubLength[i],byrow=TRUE)
        }
        l[[df$DataName[i]]]=datachunk
      }
    } else {
      if(Verbose) cat("Skipping empty data section",df$DataName[i],"\n")
    }
  }
  return(l)
}

#' Read the header of an amiramesh file
#' 
#' @param Parse Logical indicating whether to parse header (default: TRUE)
#' @export
#' @rdname amiramesh-io
#' @details \code{read.amiramesh.header} will open a connection if file is a 
#'   character vector and close it when finished reading.
read.amiramesh.header<-function(file, Parse=TRUE, Verbose=FALSE){
  if(inherits(file,"connection")) {
    con=file
  } else {
    con<-file(file, open='rt')
    on.exit(close(con))
  }
  headerLines=NULL
  while( substring(t<-readLines(con,1),1,2)!="@1"){
    headerLines=c(headerLines,t)
  }
  if(!Parse) return(headerLines)
  returnList<-list(header=headerLines)
  
  binaryfile="binary"==tolower(sub(".*(ascii|binary).*","\\1",headerLines[1],ignore.case=TRUE))
  endian=NA
  if(binaryfile){
    if(length(grep("little",headerLines[1],ignore.case=TRUE))>0) endian='little'
    else endian='big'
  }

  nHeaderLines=length(headerLines)
  # trim comments and blanks & convert all white space to single spaces
  headerLines=trim(sub("(.*)#.*","\\1",headerLines,perl=TRUE))
  headerLines=headerLines[headerLines!=""]
  headerLines=gsub("[[:space:]]+"," ",headerLines,perl=TRUE)
  
  #print(headerLines)
  # parse location definitions
  LocationLines=grep("^(n|define )(\\w+) ([0-9 ]+)$",headerLines,perl=TRUE)
  Locations=headerLines[LocationLines];headerLines[-LocationLines]
  LocationList=strsplit(gsub("^(n|define )(\\w+) ([0-9 ]+)$","\\2 \\3",Locations,perl=TRUE)," ") 
  LocationNames=sapply(LocationList,"[",1)
  Locations=lapply(LocationList,function(x) as.numeric(unlist(x[-1])))
  names(Locations)=LocationNames
  
  # parse parameters
  ParameterStartLine=grep("^\\s*Parameters",headerLines,perl=TRUE)
  if(length(ParameterStartLine)>0){
    ParameterLines=headerLines[ParameterStartLine[1]:length(headerLines)]
    returnList[["Parameters"]]<-.ParseAmirameshParameters(ParameterLines)$Parameters
    
    if(!is.null(returnList[["Parameters"]]$Materials)){
      # try and parse materials
      te<-try(silent=TRUE,{
        Ids=sapply(returnList[["Parameters"]]$Materials,'[[','Id')
        # Replace any NULLs with NAs
        Ids=sapply(Ids,function(x) ifelse(is.null(x),NA,x))
        # Note we have to unquote and split any quoted colours
        Colors=sapply(returnList[["Parameters"]]$Materials,
                      function(x) {if(is.null(x$Color)) return ('black')
                                   if(is.character(x$Color)) x$Color=unlist(strsplit(x$Color," "))
                                   return(rgb(x$Color[1],x$Color[2],x$Color[3]))})
        Materials=data.frame(id=Ids,col=I(Colors),level=seq(from=0,length=length(Ids)))
        rownames(Materials)<-names(returnList[["Parameters"]]$Materials)
      })
      if(inherits(te,'try-error')) warning("Unable to parse Amiramesh materials table")
      else returnList[["Materials"]]=Materials
    }
    
    if(!is.null(returnList[["Parameters"]]$BoundingBox)){
      returnList[["BoundingBox"]]=returnList[["Parameters"]]$BoundingBox
    }
  }
  
  # parse data definitions
  DataDefLines=grep("^(\\w+).*@(\\d+)(\\(Hx[^)]+\\)){0,1}$",headerLines,perl=TRUE)
  DataDefs=headerLines[DataDefLines];headerLines[-DataDefLines]
  HxTypes=rep("raw",length(DataDefs))
  HxLengths=rep(NA,length(DataDefs))
  LinesWithHXType=grep("(HxByteRLE|HxZip)",DataDefs)
  HxTypes[LinesWithHXType]=sub(".*(HxByteRLE|HxZip).*","\\1",DataDefs[LinesWithHXType])
  HxLengths[LinesWithHXType]=sub(".*(HxByteRLE|HxZip),([0-9]+).*","\\2",DataDefs[LinesWithHXType])
  
  # remove all extraneous chars altogether
  DataDefs=gsub("(=|@|\\}|\\{|[[:space:]])+"," ",DataDefs)
  if(Verbose) cat("DataDefs=",DataDefs,"\n")
  # make a df with DataDef info
  DataDefMatrix=matrix(unlist(strsplit(DataDefs," ")),ncol=4,byrow=T)
  # remove HxLength definitions from 4th column if required
  DataDefMatrix[HxTypes!="raw",4]=sub("^([0-9]+).*","\\1",DataDefMatrix[HxTypes!="raw",4])
  
  DataDefDF=data.frame(DataName=I(DataDefMatrix[,3]),DataPos=as.numeric(DataDefMatrix[,4]))
  
  DataDefMatrix[,1]=sub("^EdgeData$","Edges",DataDefMatrix[,1])
  # Dims will store a list of dimensions that can be used later
  DataDefDF$Dims=Locations[DataDefMatrix[,1]] 
  DataDefDF$DataLength=sapply(DataDefMatrix[,1],function(x) prod(Locations[[x]])) #  notice prod in case we have multi dim
  DataDefDF$Type=I(DataDefMatrix[,2])
  DataDefDF$SimpleType=sub("(\\w+)\\s*\\[\\d+\\]","\\1",DataDefDF$Type,perl=TRUE)
  DataDefDF$SubLength=as.numeric(sub("\\w+\\s*(\\[(\\d+)\\])?","\\2",DataDefDF$Type,perl=TRUE))
  DataDefDF$SubLength[is.na(DataDefDF$SubLength)]=1
  
  # Find size of binary data (if required?)
  TypeInfo=data.frame(SimpleType=I(c("float","byte", "ushort","short", "int", "double", "complex")),Size=c(4,1,2,2,4,8,8),
                      RType=I(c("numeric",rep("integer",4),rep("numeric",2))), Signed=c(TRUE,FALSE,FALSE,rep(TRUE,4)) )
  DataDefDF=merge(DataDefDF,TypeInfo,all.x=T)
  # Sort (just in case)
  DataDefDF= DataDefDF[order(DataDefDF$DataPos),]
  
  DataDefDF$SimpleDataLength=DataDefDF$DataLength*DataDefDF$SubLength
  DataDefDF$nBytes=DataDefDF$SubLength*DataDefDF$Size*DataDefDF$DataLength
  DataDefDF$HxType=HxTypes
  DataDefDF$HxLength=HxLengths
  DataDefDF$endian=endian
  
  # FIXME Note that this assumes exactly one blank line in between each data section
  # I'm not sure if this is a required property of the amira file format
  # Fixing this would of course require reading/skipping each data section
  nDataSections=nrow(DataDefDF)
  # NB 0 length data sections are not written
  DataSectionsLineLengths=ifelse(DataDefDF$DataLength==0,0,2+DataDefDF$DataLength)
  DataDefDF$LineOffsets=nHeaderLines+1+c(0,cumsum(DataSectionsLineLengths[-nDataSections]))
  
  returnList[["dataDef"]]=DataDefDF
  return(returnList)
}

# utility function to check that the label for a given item is unique
.checkLabel=function(l, label)   {
  if( any(names(l)==label)  ){
    newlabel=make.unique(c(names(l),label))[length(l)+1]
    warning(paste("Duplicate item",label,"renamed",newlabel))
    label=newlabel
  }
  label
}

.ParseAmirameshParameters<-function(textArray, CheckLabel=TRUE,ParametersOnly=FALSE){
  
  # First check what kind of input we have
  if(is.character(textArray)) con=textConnection(textArray,open='r')
  else {
    con=textArray
  }
  # empty list to store results
  l=list()
  
  # Should this check to see if the connection still exists?
  # in case we want to bail out sooner
  while ( {t<-try(isOpen(con),silent=TRUE);isTRUE(t) || !inherits(t,"try-error")} ){
    thisLine<-readLines(con,1)
    # no lines returned - ie end of file
    if(length(thisLine)==0) break
    
    # trim and split it up by white space
    thisLine=trim(thisLine)
    
    # skip if this is a blank line
    if(nchar(thisLine)==0) next
    
    # skip if this is a comment
    if(substr(thisLine,1,1)=="#") next
    
    items=strsplit(thisLine," ",fixed=TRUE)[[1]]
    
    if(length(items)==0) next
    
    # get the label and items
    label=items[1]; items=items[-1]
    #cat("\nlabel=",label)
    #cat("; items=",items)
    
    # return list if this is the end of a section
    if(label=="}") {
      #cat("end of section - leaving this recursion\n")
      return (l)
    }
    
    if(isTRUE(items[1]=="{")){
      # parse new subsection
      #cat("new subsection -> recursion\n")
      # set the list element!
      if(CheckLabel) label=.checkLabel(l, label)
      l[[length(l)+1]]=.ParseAmirameshParameters(con,CheckLabel=CheckLabel)
      names(l)[length(l)]<-label
      
      if(ParametersOnly && label=="Parameters")
        break # we're done
      else next
    }
    if(isTRUE(items[length(items)]=="}")) {
      returnAfterParsing=TRUE
      items=items[-length(items)]
    }
    else returnAfterParsing=FALSE
    # ordinary item
    # Check first item (if there are any items)
    if(length(items)>0){
      firstItemFirstChar=substr(items[1],1,1)
      if(any(firstItemFirstChar==c("-",as.character(0:9)) )){
        # Get rid of any commas
        items=chartr(","," ",items)
        # convert to numeric if not a string
        items=as.numeric(items)
      } else if (firstItemFirstChar=="\""){
        
        if(returnAfterParsing) thisLine=sub("\\}","",thisLine,fixed=TRUE)
        
        # dequote quoted string using scan
        items=scan(text=thisLine,what="",quiet=TRUE)[-1]
        # remove any commas
        items=items[items!=","]
        attr(items,"quoted")=TRUE
      }
    }
    # set the list element!
    if(CheckLabel)
      label=.checkLabel(l, label)
    
    l[[length(l)+1]]=items
    names(l)[length(l)]<-label
    
    if(returnAfterParsing) return(l)
  }
  # we should only get here once if we parse a valid hierarchy
  try(close(con),silent=TRUE)
  return(l)
}

# decode some raw bytes into a new raw vector of specified length
# @param bytes to decode
# @param uncompressedLength Length of the new uncompressed data
# Expects an integer array
# Structure is that every odd byte is a count
# and every even byte is the actual data
# So 127 0 127 0 127 0 12 0 12 1 0
# I think that it ends with a zero count
# -----
# in fact the above is not quite right. If >=2 consecutive bytes are different
# then a control byte is written giving the length of the run of different bytes
# and then the whole run is written out
# data can therefore only be parsed by the trick of making 2 rows if there 
# are no control bytes in range -126 to -1
decode.rle<-function(d,uncompressedLength){
  rval=raw(uncompressedLength)
  bytesRead=0
  filepos=1
  while(bytesRead<uncompressedLength){
    x=d[filepos]
    filepos=filepos+1
    if(x==0)
      stop(paste("byte at offset ",filepos," is 0!"))
    if(x>0x7f) {
      # cat("x=",x,"\n")
      x=as.integer(x)-128
      # cat("now x=",x,"\n")
      mybytes=d[filepos:(filepos+x-1)]
      filepos=filepos+x
      # that's the x that we've read
    } else {
      # x>0
      mybytes=rep.int(d[filepos],x)
      filepos=filepos+1
    }
    rval[(bytesRead+1):(bytesRead+length(mybytes))]=mybytes
    bytesRead=bytesRead+length(mybytes)
  }
  rval
}

# Uncompress zlib compressed data (from file or memory) to memory
# 
# @details zlib compressed data uses the same algorithm but a smaller header 
#   than gzip data.
# @details For connections, compressedLength must be supplied, but offset is 
#   ignored (i.e. you must seek beforehand)
# @details For files, if compressedLength is not supplied then \code{read.zlib}
#   will attempt to read until the end of the file.
# @param compressed Path to compressed file, connection or raw vector.
# @param offset Byte offset in file on disk
# @param compressedLength Bytes of compressed data to read
# @param type The compression type. See ?memDecompress for details.
# @param ... Additional parameters passed to \code{\link{readBin}}
# @return raw vector of decompressed data
# sealso memDecompress
# @export
read.zlib<-function(compressed, offset=NA, compressedLength=NA, type='gzip', ...){
  if(!is.raw(compressed)){
    if(inherits(compressed,'connection')){
      if(is.na(compressedLength)) stop("Must supply compressedLength when reading from a connection")
      con=compressed
    } else {
      con<-file(compressed,open='rb')
      on.exit(close(con))
      if(!is.na(offset)) seek(con,offset)
      else offset = 0
      if(is.na(compressedLength)) compressedLength=file.info(compressed)$size-offset
    }
    compressed=readBin(con, what=raw(), n=compressedLength)
  }
  memDecompress(compressed, type=type, ...)
}

# Compress raw data, returning raw vector or writing to file
# 
# @details The default value of \code{con=raw()} means that this function will 
#   return a raw vector of compressed data if con is not specified.
# @param uncompressed \code{raw} vector of data
# @param con Raw vector or path to output file
# @return A raw vector (if \code{con} is a raw vector) or invisibly NULL.
# @seealso Depends on \code{\link{memCompress}}
# @export
write.zlib<-function(uncompressed, con=raw()){
  if(!inherits(con, "connection") && !is.raw(con)){
    con=open(con, open='wb')
    on.exit(close(con))
  }
  d=memCompress(uncompressed, type='gzip')
  if(is.raw(con)) return(d)
  writeBin(object=d,con=con)
}

#' Check if file is amiramesh format
#' 
#' @details Tries to be as fast as possible by reading only first 11 bytes and 
#'   checking if they equal to "# AmiraMesh" or (deprecated) "# HyperMesh".
#' @param f Path to one or more files to be tested \strong{or} an array of raw 
#'   bytes, for one file only.
#' @param bytes optional raw vector of at least 11 bytes from the start of a 
#'   single file (used in preference to reading file \code{f}).
#' @return logical
#' @export
#' @family amira
is.amiramesh<-function(f=NULL, bytes=NULL) {
  if(!is.null(bytes) && is.character(f) && length(f)>1)
    stop("Can only check bytes for a single file")
  tocheck=if(is.null(bytes)) f else bytes
  generic_magic_check(tocheck, c("# HyperMesh", "# AmiraMesh"))
}

#' Return the type of an amiramesh file on disk or a parsed header
#' 
#' @details Note that when checking a file we first test if it is an amiramesh 
#'   file (fast, especially when \code{bytes!=NULL}) before reading the header 
#'   and determining content type (slow).
#' @param x Path to files on disk or a single pre-parsed parameter list
#' @param bytes A raw vector containing at least 11 bytes from the start of the
#'   file.
#' @return character vector (NA_character_ when file invalid)
#' @export
#' @family amira
amiratype<-function(x, bytes=NULL){
  if(is.list(x)) h<-x
  else {
    # we have a file, optionally with some raw data
    if(!is.null(bytes) && length(x)>1) 
      stop("Can only accept bytes argument for single file")
    if(length(x)>1) return(sapply(x,amiratype))
    
    if(is.null(bytes) || length(bytes)<14) {
      f=gzfile(x, open='rb')
      on.exit(close(f))
      bytes=readBin(f, what=raw(), n=14L)
    }
    
    if(!isTRUE(is.amiramesh(bytes))) {
      if(generic_magic_check(bytes, "# HyperSurface")) {
        return("HxSurface")
      } else return(NA_character_)
    }
    h=try(read.amiramesh.header(x, Verbose=FALSE, Parse = F), silent=TRUE)
    if(inherits(h,'try-error')) return(NA_character_)
  }

  ct=grep("ContentType", h, value = T, fixed=T)
  if(length(ct)){
    ct=sub(".*ContentType","",ct[1])
    ct=gsub("[^A-z ]+"," ",ct)
    ct=scan(text=ct, what = "", quiet = T)
    if(length(ct)==0) stop('unable to parse ContentType')
    return(ct[1])
  }
  ct=grep("CoordType", h, value = T, fixed=T)
  if(length(ct)){
    ct=sub(".*CoordType","",ct[1])
    ct=gsub("[^A-z ]+"," ",ct)
    ct=scan(text=ct, what = "", quiet = T)
    if(length(ct)==0) stop('unable to parse CoordType')
    return(paste0(ct[1], ".field"))
  }
  NA_character_
}

# generic function to return a function tha identifies an amira type
is.amiratype<-function(type) {
  function(f, bytes=NULL){
    rval=amiratype(f, bytes=bytes)
    sapply(rval, function(x) isTRUE(x==type))
  }
}

#' Write a 3D data object to an amiramesh format file
#' @inheritParams write.im3d
#' @param enc Encoding of the data. NB "raw" and "binary" are synonyms.
#' @param dtype Data type to write to disk
#' @param endian Endianness of data block. Defaults to current value of 
#'   \code{.Platform$endian}.
#' @param WriteNrrdHeader Whether to write a separate detached nrrd header next 
#'   to the amiramesh file allowing it to be opened by a NRRD reader. See 
#'   details.
#' @details Note that only \code{'raw'} or \code{'text'} format data can
#'   accommodate a detached NRRD format header since Amira's HxZip format is
#'   subtly different from NRRD's gzip encoding. There is a full description 
#'   of the deteached NRRD format in the help for \code{\link{write.nrrd}}.
#' @export
#' @seealso \code{\link{.Platform}, \link{read.amiramesh}, \link{write.nrrd}}
#' @examples
#' d=array(rnorm(1000), c(10, 10, 10))
#' tf=tempfile(fileext='.am')
#' write.amiramesh(im3d(d, voxdims=c(0.5,0.5,1)), file=tf, WriteNrrdHeader=TRUE)
#' d2=read.nrrd(paste(tf, sep='', '.nhdr'))
#' all.equal(d, d2, tol=1e-6)
write.amiramesh<-function(x, file, enc=c("binary","raw","text","hxzip"),
                          dtype=c("float","byte", "short", "ushort", "int", "double"),
                          endian=.Platform$endian, WriteNrrdHeader=FALSE){
  enc=match.arg(enc)
  endian=match.arg(endian, c('big','little'))
  if(enc=='text') cat("# AmiraMesh ASCII 1.0\n\n",file=file)
  else if(endian=='little') cat("# AmiraMesh BINARY-LITTLE-ENDIAN 2.1\n\n",file=file)
  else cat("# AmiraMesh 3D BINARY 2.0\n\n",file=file)
  
  fc=file(file,open="at") # ie append, text mode
  cat("# Created by write.amiramesh\n\n",file=fc)	
  
  if(!is.list(x)) d=x else d=x$estimate
  # Find data type and size for amira
  dtype=match.arg(dtype)	
  dtypesize<-c(4,1,2,2,4,8)[which(dtype==c("float","byte", "short","ushort", "int", "double"))]
  # Set the data mode which will be used in the as.vector call at the
  # moment that the binary data is written out.
  if(dtype%in%c("byte","short","ushort","int")) dmode="integer"
  if(dtype%in%c("float","double")) dmode="numeric"
  
  lattice=dim(d)
  cat("define Lattice",lattice,"\n",file=fc)
  
  cat("Parameters { CoordType \"uniform\",\n",file=fc)
  # note Amira's definition for the bounding box:
  # the range of the voxel centres.
  # So eval.points should correspond to the CENTRE of the
  # voxels at which the density is evaluated
  cat("\t# BoundingBox is xmin xmax ymin ymax zmin zmax\n",file=fc)
  BoundingBox=NULL
  if(!is.null(attr(x,"BoundingBox"))){
    BoundingBox=attr(x,"BoundingBox")
  } else if(is.list(d) && !is.null(d$eval.points)){
    BoundingBox=as.vector(apply(d$eval.points,2,range))
  }
  if(!is.null(BoundingBox)) cat("\t BoundingBox",BoundingBox,"\n",file=fc)
  cat("}\n\n",file=fc)
  
  if(enc=="hxzip"){
    raw_data=writeBin(as.vector(d,mode=dmode),raw(),size=dtypesize,endian=endian)
    zlibdata=write.zlib(raw_data)
    cat("Lattice { ",dtype," ScalarField } = @1(HxZip,",length(zlibdata),")\n\n",sep="",file=fc)
  } else cat("Lattice {",dtype,"ScalarField } = @1\n\n",file=fc)
  
  cat("@1\n",file=fc)
  close(fc)
  
  # Write a Nrrd header to accompany the amira file if desired
  # see http://teem.sourceforge.net/nrrd/
  if(WriteNrrdHeader) {
    if(enc=="hxzip") stop("Nrrd cannot handle Amira's HxZip encoding (which is subtly different from gzip)")
    nrrdfile=paste(file,sep=".","nhdr")
    cat("NRRD0004\n",file=nrrdfile)
    fc=file(nrrdfile,open="at") # ie append, text mode
    nrrdType=ifelse(dtype=="byte","uint8",dtype)
    
    cat("encoding:", ifelse(enc=="text","text","raw"),"\n",file=fc)
    cat("type: ",nrrdType,"\n",sep="",file=fc)
    cat("endian: ",endian,"\n",sep="",file=fc)
    # Important - this sets the offset in the amiramesh file from which
    # to start reading data
    cat("byte skip:",file.info(file)$size,"\n",file=fc)
    cat("dimension: ",length(lattice),"\n",sep="",file=fc)
    cat("sizes:",lattice,"\n",file=fc)
    voxdims=voxdims(x)
    if(!is.null(voxdims)) cat("spacings:",voxdims,"\n",file=fc)
    if(!is.null(BoundingBox)){
      cat("axis mins:",matrix(BoundingBox,nrow=2)[1,],"\n",file=fc)
      cat("axis maxs:",matrix(BoundingBox,nrow=2)[2,],"\n",file=fc)
    }
    cat("data file: ",basename(file),"\n",sep="",file=fc)
    cat("\n",file=fc)
    close(fc)
  }
  
  if(enc=='text'){
    write(as.vector(d, mode=dmode), ncolumns=1, file=file, append=TRUE)
  } else {
    fc=file(file,open="ab") # ie append, bin mode
    if(enc=="hxzip")
      writeBin(zlibdata, fc, size=1, endian=endian)
    else
      writeBin(as.vector(d, mode=dmode), fc, size=dtypesize, endian=endian)
    close(fc)
  }
}

