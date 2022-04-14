read.elastixreg <- function(x, ...) {
  l=read.elastix(x)
  if(isTRUE(l$Transform=='BSplineTransform')) {
    l$TransformParameters=matrix(l$TransformParameters, ncol=3, byrow=FALSE)
  }
  # reginmem is supposed to help handle the situation when we have read the 
  # registration into memory
  class(l)=union(c('elastixreg', 'reginmem', 'reg'), class(l))
  l
}

# internal function to read the elastix files into an R list
read.elastix<-function(con, CheckLabel=TRUE){
  
  l=list()
  
  if(is.character(con)) {
    filename=con
    con=file(filename,'rt')
    on.exit(close(con))
    t=readLines(filename,1)
    if( !any(grep("(Transform",t[1],fixed=TRUE,useBytes=TRUE)) )
      stop(paste("This doesn't appear to be an elastix registration:",filename))
  }
  
  checkLabel=function(label) {
    if( any(names(l)==label)  ){
      newlabel=make.unique(c(names(l),label))[length(l)+1]
      warning(paste("Duplicate item",label,"renamed",newlabel))
      label=newlabel
    }
    label
  }
  
  removeBrackets <- function(x) {
    if(nchar(x)<2) stop("Invalid line!")
    if(substr(x,1,1)!="(") stop("No opening bracket!")
    if(substr(x,nchar(x),nchar(x))!=")") stop("No closing bracket!")
    substr(x, 2, nchar(x)-1L)
  }
  
  # Should this check to see if the connection still exists?
  # in case we want to bail out sooner
  while ( isTRUE(isOpen(con)) ){
    thisLine<-readLines(con,1)
    # no lines returned - ie end of file
    if(length(thisLine)==0) break
    
    # trim and split it up by white space
    thisLine=trimws(thisLine)
    
    # skip if this is a blank line
    if(nchar(thisLine)==0) next
    # skip if this is a comment
    if(isTRUE(substr(thisLine, 1, 2) == "//")) next
    
    thisLine=removeBrackets(thisLine)
    items=strsplit(thisLine," ",fixed=TRUE)[[1]]
    
    if(length(items)==0) next
    # get the label and items
    label=items[1]; items=items[-1]
    
    firstItemFirstChar=substr(items[1],1,1)
    if (firstItemFirstChar=="\""){
      # dequote quoted string
      # can do this by using a textConnection
      tc=textConnection(thisLine)
      items=scan(tc,what="",quiet=TRUE)[-1]
      close(tc)
      # attr(items,"quoted")=TRUE
    } else {
      items=as.numeric(items)
    }
    
    # set the list element!
    if(CheckLabel)
      label=checkLabel(label)
    l[[label]]=items  
  }
  
  if(isTRUE(try(file.exists(filename)))){
    attr(l,"file.info")=file.info(filename)
  }
  return(l)
}

