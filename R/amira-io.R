ReadAmiramesh<-function(filename,DataSectionsToRead=NULL,Verbose=FALSE,AttachFullHeader=FALSE,Simplify=TRUE,endian){
  # attempt to write a generic amiramesh reader
  firstLine=readLines(filename,n=1)
  if(!any(grep("#\\s+(amira|hyper)mesh",firstLine,ignore.case=TRUE))){
    warning(paste(filename,"does not appear to be an AmiraMesh file"))
    return(NULL)
  }
  binaryfile="binary"==tolower(sub(".*(ascii|binary).*","\\1",firstLine,ignore.case=TRUE))
  
  if(binaryfile && missing(endian)){
    if(length(grep("little",firstLine,ignore.case=TRUE))>0) endian='little'
    else endian='big'
  }
  
  con=if(binaryfile) file(filename,open='rb') else file(filename,open='rt')
  on.exit(try(close(con),silent=TRUE))
  header=ReadAmiramesh.Header(con,Verbose=Verbose,CloseConnection=FALSE)
  parsedHeader=header[["dataDef"]]
  
  if(is.null(DataSectionsToRead)) DataSectionsToRead=parsedHeader$DataName
  else DataSectionsToRead=intersect(parsedHeader$DataName,DataSectionsToRead)	
  if(binaryfile){
    filedata=.ReadAmiramesh.BinaryData(con,parsedHeader,DataSectionsToRead,Verbose=Verbose,endian=endian)
    close(con)
  } else {
    close(con)
    #print(parsedHeader)
    filedata=.ReadAmiramesh.ASCIIDataFully(filename,parsedHeader,DataSectionsToRead,Verbose=Verbose)
    #cat(length(filedata))
  }
  
  if(!AttachFullHeader) header=header[setdiff(names(header),c("header"))]	
  for (n in names(header))
    attr(filedata,n)=header[[n]]
  
  if(Simplify && length(filedata)==1){
    filedata2=filedata[[1]]
    attributes(filedata2)=attributes(filedata)
    dim(filedata2)=dim(filedata[[1]])
    filedata=filedata2
  } 
  return(filedata)
}

.ReadAmiramesh.BinaryData<-function(con,df,DataSectionsToRead,Verbose=TRUE,endian=endian){
  #print(df)
  l=list()
  # TODO
  for(i in seq(len=nrow(df))){
    if(Verbose) cat("Current offset is",seek(con),";",df$nBytes[i],"to read\n")
    
    if(all(DataSectionsToRead!=df$DataName[i])){
      # Just skip this section
      if(Verbose) cat("Skipping data section",df$DataName[i],"\n")
      seek(con,df$nBytes[i],origin="current")
    } else {
      if(Verbose) cat("Reading data section",df$DataName[i],"\n")
      if(df$HxType[i]=="HxByteRLE"){
        d=readBin(con,what=raw(0),n=as.integer(df$HxLength[i]),size=1)
        d=DecodeRLEBytes(d,df$SimpleDataLength[i])
        x=as.integer(d)
      } else {
        if(df$RType[i]=="integer") whatval=integer(0) else whatval=numeric(0)
        x=readBin(con,df$SimpleDataLength[i],size=df$Size[i],what=whatval,signed=df$Signed[i],endian=endian)
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
    readLines(con,n=1) # Skip return at end of section
    nextSectionHeader=readLines(con,n=1)
    if(Verbose) cat("nextSectionHeader = ",nextSectionHeader,"\n")
  }
  l
}

#' Read ASCII AmiraMesh data without assuming anything about line spacing
#' between sections
#' @param filename file (or connection) to read
#' @param df dataframe containing details of data in file
#' @param DataSectionsToRead character vector containing names of sections
#' @param Verbose Print status messages
#' @return list of named data chunks
#' @author jefferis
.ReadAmiramesh.ASCIIDataFully<-function(filename,df,DataSectionsToRead,Verbose=TRUE){
  l=list()
  #  df=subset(df,DataName%in%DataSectionsToRead)
  df=df[order(df$DataPos),]
  if(inherits(filename,'connection')) 
    con=filename
  else {
    # rt is essential to ensure that readLines behaves with gzipped files
    con=file(filename,open='rt')
    on.exit(close(con))
  }
  readLines(con, df$LineOffsets[1]-1)
  for(i in seq(len=nrow(df))){
    if(df$DataLength[i]>0){
      # read some lines until we get to a data section
      nskip=0
      while( substring(t<-readLines(con,1),1,1)!="@"){nskip=nskip+1}
      if(Verbose) cat("Skipped",nskip,"lines to reach next data section")
      if(Verbose) cat("Reading ",df$DataLength[i],"lines in file",filename,"\n")
      
      if(df$RType[i]=="integer") whatval=integer(0) else whatval=numeric(0)
      datachunk=scan(con,what=whatval,n=df$SimpleDataLength[i],quiet=!Verbose)
      # store data if required
      if(df$DataName[i]%in%DataSectionsToRead){
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

trim =function(t) sub('[[:space:]]+$', '', sub('^[[:space:]]+', '', t))

ReadAmiramesh.Header<-function(con,Verbose=TRUE,CloseConnection=TRUE){
  headerLines=NULL
  if(!inherits(con,"connection")) con<-file(con,open='rt')
  if(CloseConnection) on.exit(close(con))
  while( substring(t<-readLines(con,1),1,2)!="@1"){
    headerLines=c(headerLines,t)
  }
  returnList<-list(header=headerLines)
  
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

.ParseAmirameshParameters<-function(textArray, CheckLabel=TRUE,ParametersOnly=FALSE){
  
  # First check what kind of input we have
  closeConnectionWhenDone=TRUE
  if(is.character(textArray)) con=textConnection(textArray,open='r')
  else {
    con=textArray
    closeConnectionWhenDone=FALSE
  }
  # empty list to store results
  l=list()
  
  # utility function to check that the label for a given item is unique
  checkLabel=function(label) 	{
    if( any(names(l)==label)  ){
      newlabel=make.unique(c(names(l),label))[length(l)+1]
      warning(paste("Duplicate item",label,"renamed",newlabel))
      label=newlabel
    }
    label
  }
  
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
      if(CheckLabel) label=checkLabel(label)
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
        
        # dequote quoted string
        # can do this by using a textConnection
        tc=textConnection(thisLine)
        
        items=scan(tc,what="",quiet=TRUE)[-1]
        close(tc)
        attr(items,"quoted")=TRUE
      }
    }
    # set the list element!
    if(CheckLabel)
      label=checkLabel(label)
    
    l[[length(l)+1]]=items
    names(l)[length(l)]<-label
    
    if(returnAfterParsing) return(l)
  }
  # we should only get here once if we parse a valid hierarchy
  try(close(con),silent=TRUE)
  return(l)
}
