ReadNrrdHeader<-function(filename,Verbose=TRUE,CloseConnection=TRUE){
  nrrdspec=list()
  if(!inherits(filename,"connection")){
    con<-file(filename,open='rt')
    attr(nrrdspec,"path")=filename # store filename
  } else con=filename
  
  if(CloseConnection) on.exit(close(con))
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

NrrdDataFiles<-function(nhdr,ReturnAbsPath=TRUE){
  if(!is.list(nhdr)){
    # we need to read in the nrrd header
    if(length(nhdr)>1) return(sapply(nhdr,NrrdDataFiles))
    if(!is.nrrd(nhdr)) stop("This is not a nrrd file")
    h=ReadNrrdHeader(nhdr)
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
