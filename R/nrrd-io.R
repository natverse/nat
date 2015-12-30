#' Read nrrd file into an array in memory
#' 
#' @details \code{read.nrrd} reads data into a raw array. If you wish to 
#'   generate a \code{\link{im3d}} object that includes spatial calibration (but
#'   is limited to representing 3D data) then you should use
#'   \code{\link{read.im3d}}.
#'   
#'   ReadByteAsRaw=unsigned (the default) only reads unsigned byte data as a raw
#'   array. This saves quite a bit of space and still allows data to be used for
#'   logical indexing.
#' @param file Path to a nrrd (or a connection for \code{read.nrrd.header})
#' @param origin Add a user specified origin (x,y,z) to the returned object
#' @param ReadData When FALSE just return attributes (i.e. the nrrd header)
#' @param AttachFullHeader Include the full nrrd header as an attribute of the 
#'   returned object (default TRUE)
#' @param ReadByteAsRaw Either a character vector or a logical vector specifying
#'   when R should read 8 bit data as an R \code{raw} vector rather than 
#'   \code{integer} vector.
#' @param Verbose Status messages while reading
#' @return An \code{array} object, optionally with attributes from the nrrd 
#'   header.
#' @export
#' @seealso \code{\link{write.nrrd}}, \code{\link{read.im3d}}
read.nrrd<-function(file, origin=NULL, ReadData=TRUE, AttachFullHeader=TRUE,
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
    if(!is.null(h$lineskip)) {
      readLines(fc, n=h$lineskip)
    }
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
      if(!is.null(h$byteskip)){
        if(isSeekable(fc)) {
          seek(fc, where=h$byteskip, origin='current')
        } else {
          readBin(fc, what='raw', n=h$byteskip)
        }
      }
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

nrrd.datafiles<-function(nhdr, full.names=TRUE){
  if(!is.list(nhdr)){
    # we need to read in the nrrd header
    if(length(nhdr)>1) 
      return(sapply(nhdr, nrrd.datafiles, full.names=full.names, 
                    simplify = FALSE))
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
  
  if(full.names){
    # check if paths begin with /
    relpaths=substring(dfs,1,1)!="/"
    if(any(relpaths)){
      nhdrpath=attr(h,"path")
      if(is.null(nhdrpath))
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
#'   
#'   Will produce a warning if no valid dimensions can be found.
#' @param file path to nrrd/nhdr file or a list containing a nrrd header
#' @param ReturnAbsoluteDims Defaults to returning absolute value of dims even 
#'   if there are any negative space directions
#' @return numeric vector of voxel dimensions (\code{NA_real_} when missing) of
#'   length equal to the image dimension.
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
    voxdims=rep(NA_real_,h$dimension)
  }
  
  # Sometimes get -ve space dirs, take abs if requested
  if(ReturnAbsoluteDims) abs(voxdims)
  else voxdims
}

#' Write data and metadata to NRRD file or create a detached NRRD (nhdr) file.
#' 
#' @description \code{write.nrrd} writes an array, vector or im3d object to a 
#'   NRRD file. When \code{x} is an \code{im3d} object, appropriate spatial 
#'   calibration fields are added to the header.
#'   
#' @section Detached NRRDs: NRRD files can be written in \emph{detached} format 
#'   (see \url{http://teem.sourceforge.net/nrrd/format.html#detached}) in which 
#'   a text \bold{nhdr} file is used to described the contents of a separate 
#'   (usually binary) data file. This means that the nhdr file can be inspected 
#'   and edited with a text editor, while the datablock can be in a completely 
#'   raw format that can be opened even by programs that do not understand the 
#'   NRRD format. Furthermore detached NRRD header files can be written to 
#'   accompany non-NRRD image data so that it can be opened by nrrd readers.
#'   
#'   If \code{file} has extension \code{.nhdr} \emph{or} \code{datafile} is 
#'   non-NULL, then \code{write.nrrd} will write a separate datafile. If 
#'   \code{datafile} is set, then it is interpeted as specifying a path relative
#'   to the \bold{nhdr} file. If \code{datafile} is not specified then default 
#'   filenames will be chosen according to the encoding following the 
#'   conventions of the teem library:
#'   
#'   \itemize{
#'   
#'   \item raw \code{'<nhdrstem>.raw'}
#'   
#'   \item gzip \code{'<nhdrstem>.raw.gz'}
#'   
#'   \item text \code{'<nhdrstem>.ascii'}
#'   
#'   }
#' @section Data file paths: When a detached NRRD is written, the 
#'   \code{datafile} can be specified either as \emph{relative} or 
#'   \emph{absolute} path. Relative paths are strongly recommended - the best 
#'   place is right next to the datafile. Relative paths are always specified 
#'   with respect to the location of the \bold{nhdr} file.
#'   
#'   The \code{datafile} argument is not processed by \code{write.nrrd} so it is
#'   up to the caller to decide whether a relative or absolute path will be 
#'   used.
#'   
#'   For \code{write.nrrd.header.for.file} if \code{outfile} is not specified 
#'   then the nhdr file will be placed next to the original image stack and the 
#'   \code{datafile} field will therefore just be \code{basename(infile)}. If 
#'   outfile is specified explicitly, then \code{datafile} will be set to the 
#'   full path in the \code{infile} argument. Therefore if you wish to specify 
#'   \code{outfile}, you \emph{must} set the current working directory (using 
#'   \code{setwd}) to the location in which \code{outfile} will be written to 
#'   ensure that the path to the datafile is correct. A future TODO would add 
#'   the ability to convert an absolute datafile path to a relaive one (by
#'   finding the common path between datafile and nhdr folders).
#'   
#' @section Header: For \code{write.nrrd}, arguments \code{enc}, \code{dtype}, 
#'   and \code{endian} along with the dimensions of the input (\code{x}) will 
#'   override the corresponding NRRD header fields from any supplied 
#'   \code{header} argument. See 
#'   \url{http://teem.sourceforge.net/nrrd/format.html} for details of the NRRD 
#'   fields.
#'   
#' @param x Data to write as an \code{array}, \code{vector} or 
#'   \code{\link{im3d}} object.
#' @param file Character string naming an output file (a detached nrrd header 
#'   when \code{file} has extension 'nhdr').
#' @param enc One of three supported nrrd encodings ("gzip", "raw", "text")
#' @param dtype The data type to write. One of "float","byte", "short", 
#'   "ushort", "int", "double"
#' @param endian One of "big" or "little". Defaults to \code{.Platform$endian}.
#' @param header List containing fields of nrrd header - see \emph{Header} 
#'   section.
#' @param datafile Optional name of separate file into which data should be 
#'   written (see details).
#' @export
#' @seealso \code{\link{read.nrrd}, \link{.Platform}}
write.nrrd<-function(x, file, enc=c("gzip","raw","text"),
                     dtype=c("float","byte", "short", "ushort", "int", "double"),
                     header=attr(x,'header'), endian=.Platform$endian,
                     datafile=NULL){
  ## handle core arguments
  enc=match.arg(enc)
  endian=match.arg(endian, c('big','little'))
  dtype=match.arg(dtype)
  nrrdDataTypes=c(byte="uint8",ushort="uint16",short="int16",int="int32",
                  float="float",double="double")
  nrrdDataType=nrrdDataTypes[dtype]
  if(is.na(nrrdDataType))
    stop("Unable to write nrrd file for data type: ",dtype)
  
  ## is this a detached nrrd
  ext=tools::file_ext(file)
  if(ext=='nhdr' && is.null(datafile)){
    # these are the extensions used by unu
    dext=switch(enc, raw='.raw', gzip='.raw.gz', text='.ascii')
    # NB we will put the datafile next to the nhdr file
    datafile=paste0(basename(tools::file_path_sans_ext(file)), dext)
  }
  
  ## set up core header fields
  goodmodes=c("logical", "numeric", "character", "raw")
  h=list(type=nrrdDataType, encoding=enc, endian=endian)
  if(is.array(x) || is.im3d(x)) {
    h$dimension=length(dim(x))
    h$sizes=dim(x)
  } else if(mode(x) %in% goodmodes) {
    h$dimension=1
    h$sizes=length(x)
  } else {
    stop("write.nrrd only accepts arrays/matrices (including im3d) and vectors",
         " of mode: ", paste(goodmodes, collapse = " "))
  }
  
  # Find data type and size for nrrd
  dtypesize<-c(4,1,2,2,4,8)[which(dtype==c("float","byte", "short","ushort", 
                                           "int", "double"))]
  # Set the data mode which will be used in the as.vector call at the
  # moment that the binary data is written out.
  if(dtype%in%c("byte","short","ushort","int")) dmode="integer"
  if(dtype%in%c("float","double")) dmode="numeric"
  
  if(is.im3d(x)) {
    im3dh=im3d2nrrdheader(x)
    new_fields=setdiff(names(im3dh), names(h))
    h[new_fields]=im3dh[new_fields]
  }
  if(!is.null(header)) {
    new_fields=setdiff(names(header), names(h))
    h[new_fields]=header[new_fields]
  }
  
  # remove encoding field if not required before writing
  if(h$encoding=='text' || dtypesize==1) h$endian=NULL
  
  # process datafile as last field in header
  h$datafile=datafile

  write.nrrd.header(h, file)
  
  # set things up for detached nrrd or regular nrrd
  if(is.null(datafile)) {
    # regular nrrd, append to file containing header
    fmode='ab'
  } else {
    # detached nrrd, (over)write separate datafile
    fmode='wb'
    # set working dir to location of nhdr to simplify interpretation of datafile
    owd=setwd(dirname(file))
    file=datafile
    on.exit(setwd(owd), add = TRUE)
  }
  
  # nothing to write, so assume we just wanted to write the header
  if(length(x)==0) 
    return(invisible(NULL))
  
  if(enc=='text'){
    write(as.vector(x,mode=dmode),ncolumns=1,file=file,append=fmode=='ab')
  } else {
    fc=ifelse(enc=="gzip", gzfile, file)(file, open=fmode)
    writeBin(as.vector(x, mode=dmode), fc, size=dtypesize, endian=endian)
    close(fc)
  }
}

#' @description \code{write.nrrd.header} writes a nrrd header file.
#' @export
#' @rdname write.nrrd
write.nrrd.header <- function (header, file) {
  # helper function
  nrrdvec=function(x) sprintf("(%s)",paste(x,collapse=","))
  cat("NRRD0004\n", file=file)
  for(n in names(header)) {
    f=header[[n]]
    # special handling for a couple of fields
    if(n=='space origin' ) {
      f=nrrdvec(f)
    } else if(n=='space directions') {
      f=apply(f, 1, nrrdvec)
    }
    if(length(f)>1) f=paste(f, collapse = " ")
    cat(paste0(n, ": ", f ,"\n"), file=file, append=TRUE)
  }
  # Single blank line terminates header
  cat("\n", file=file, append=TRUE)
}

#' @description \code{write.nrrd.header.for.file} makes a detached NRRD
#'   (\bold{nhdr}) file that points at another image file on disk, making it
#'   NRRD compatible. This can be a convenient way to make NRRD inputs for other
#'   tools e.g. CMTK and also allows the same data block to pointed to by
#'   different nhdr files with different spatial calibration.
#' @rdname write.nrrd
#' @param infile,outfile Path to input and output file for 
#'   \code{write.nrrd.header.for.file}. If \code{outfile} is \code{NULL} (the
#'   default) then it will be set to \code{<infilestem.nhdr>}.
#' @export
write.nrrd.header.for.file<-function(infile, outfile=NULL) {
  if(is.null(outfile)){
    outfile=paste0(tools::file_path_sans_ext(infile),".nhdr")
    datafile=basename(infile)
  } else {
    datafile=infile
  }
    
  x=read.im3d(infile, ReadData = FALSE)
  if(!is.null(dd<-attr(x,'dataDef'))){
    if(dd$HxType!='raw')
      stop("only raw format Amiramesh files are nrrd compatible!")
    if(nrow(dd)>1)
      stop("I only accept Amiramesh files with one data block")
    write.nrrd(x, outfile, enc = 'raw', dtype = dd$SimpleType, endian = dd$endian, 
               datafile = datafile, header=list(lineskip=dd$LineOffsets))
  } else if(!is.null(nh<-attr(x,'header'))) {
    # assume that we are dealing with a nrrd
    # skip 1 extra line because of terminating blank line
    nh$lineskip=length(attr(nh,"headertext"))+1
    nh$datafile=datafile
    write.nrrd.header(header = nh, file = outfile)
  } else {
    stop("I don't know how to make a detached nrrd for this image type")
  }
  outfile
}

# internal function to make key spatial nrrd header fields from im3d object
im3d2nrrdheader<-function(x) {
  if(!is.im3d(x)) stop("x is not an im3d object!")
  h=list(dimension=length(dim(x)), sizes=dim(x))
  # for im3d assume that space dimension is same as array dimension
  h$`space dimension`=length(dim(x))
  # nb not origin(x) since that will return (0,0,0) if missing
  h$`space origin`=attr(x, 'origin')
  h$`space directions`=diag(voxdims(x))
  h
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
