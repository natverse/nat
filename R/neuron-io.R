#' Read a single neuron from a file
#' 
#' @details This function will handle \code{neuron} and \code{dotprops} objects 
#'   saved in R .rds or .rda format by default. Additional file formats can be
#'   registered using \code{neuronformats}.
#' @export
#' @param f Path to file
#' @param ... additional arguments passed to format-specific readers
#' @seealso \code{\link{read.neurons}, \link{neuronformats}}
read.neuron<-function(f, ...){
  #if(!file.exists(f)) stop("Unable to read file: ",f)
  ext=tolower(sub(".*\\.([^.]+$)","\\1",basename(f)))
  if(ext=="rds")
    n=readRDS(f)
  else if(ext=="rda"){
    objname=load(f,envir=environment())
    if(length(objname)>1) stop("More than 1 object in file:",f)
    n=get(objname,envir=environment())
  } else {
    ffs=getformatfuns(f,action='read')
    if(is.null(ffs)) stop("Unable to identify file type of:", f)
    n=ffs$read(f)
  }
  # we can normally rely on dotprops objects to have the correct class
  if(is.neuron(n,Strict=FALSE) && !is.dotprops(n)) as.neuron(n)
  else n
}

#' Set or return list of registered file formats that we can read
#' 
#' @param format Character vector naming the format
#' @param ext Character vector of file extensions
#' @param read,write Functions to read and write this format
#' @param magic Function to test whether a file is of this format
#' @param magiclen Optional integer specifying maximum number of bytes required
#' from file header to determine file's type.
#' @param class The S3 class for the format (character vector e.g. 'neuron')
neuronformats<-function(format,ext=format,read=NULL,write=NULL,magic=NULL,
                        magiclen=NA_integer_,class=NULL){
  currentformats=ls(envir=.neuronformats)
  if(missing(format)){
    if(!is.null(class)) 
      return(Filter(function(x) isTRUE(
        get(x,envir=.neuronformats)$class%in%class),currentformats))
    else return(currentformats)
  }
  
  if(format%in%currentformats) warning("This format has already been registered")
  if(is.null(read) && is.null(write)) stop("Must be provide at least one read or write function")
  assign(format,list(ext=ext,read=read,write=write,magic=magic,magiclen=magiclen,
                     class=class),
         envir=.neuronformats)
  invisible()
}

#' Get the list of functions (read, write etc) for a file
#' @rdname neuronformats
#' @param f Path to a file
#' @param action Whether we must have a read or write function for this file
getformatfuns<-function(f, action=c('read','write'), class=NULL){
  action=match.arg(action)
  
  formatsforclass<-neuronformats(class=class)
  if(!length(formatsforclass)) return(NULL)
  
  magiclens=sapply(formatsforclass,function(f) get(f,envir=.neuronformats)$magiclen)
  max_magiclen=max(c(-Inf,magiclens),na.rm=TRUE)
  magic=if(is.finite(max_magiclen)) readBin(f,what=raw(),n=max_magiclen) else NULL
  ext=tolower(sub(".*\\.([^.]+$)","\\1",basename(f)))
  for(format in formatsforclass){
    ffs=get(format,envir=.neuronformats)
    
    # check that we have a read or write function for this format
    if (!action%in%names(ffs)) next
    
    if(!is.null(ffs$magic)){
      # we have a magic function for this file, so check by magic
      if(ffs$magic(magic)) return(ffs)
    } else {
      # else check by file extension
      if(ext%in%ffs$ext) return(ffs)
    }
  }
  return(NULL)
}

#' read a neuron in swc file format
read.neuron.swc<-function(f){
  message("swc reader not yet implemented!")
  NULL
}

#' read a neuron in neurolucida file format
read.neuron.neurolucida<-function(f){
  message("neurolucida reader not yet implemented!")
  NULL
}

