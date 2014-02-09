#' Read a single neuron from a file
#' 
#' @details This function will handle \code{neuron} and \code{dotprops} objects 
#'   saved in R .rds or .rda format by default. Additional file formats can be 
#'   registered using \code{fileformats}.
#' @export
#' @param f Path to file
#' @param ... additional arguments passed to format-specific readers
#' @seealso \code{\link{read.neurons}, \link{fileformats}}
#' @examples
#' # note that we override the default NeuronName field
#' n=read.neuron(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'),
#'   NeuronName="EBT7R")
#' # use a function to set the NeuronName field
#' n3=read.neuron(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'),
#'   NeuronName=function(x) sub("\\..*","",x))
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
    ffs=getformatreader(f)
    if(is.null(ffs)) stop("Unable to identify file type of:", f)
    n=match.fun(ffs$read)(f, ...)
  }
  # we can normally rely on dotprops objects to have the correct class
  if(is.neuron(n,Strict=FALSE) && !is.dotprops(n)) as.neuron(n)
  else n
}

#' Read one or more neurons from file to a neuronlist in memory
#' 
#' @details This function will cope with the same set of file formats offered by
#'   \code{read.neuron}. If the \code{paths} argument specifies a (single)
#'   directory then all files in that directory will be read unless an optional
#'   regex pattern is also specified. \code{neuronnames} must specify a unique
#'   set of names that will be used as the names of the neurons in the resultant
#'   neuronlist. If \code{neuronnames} is a a function then this will be applied
#'   to the path to each neuron. The default value is the function
#'   \code{basename} which results in each neuron being named for the input file
#'   from which it was read. The optional dataframe (\code{df}) detailing each
#'   neuron should have \code{rownames} that match the names of each neuron. It
#'   would also make sense if the same key was present in a column of the data
#'   frame. If the dataframe contains more rows than neurons, the superfluous
#'   rows are dropped with a warning. If the dataframe is missing rows for some
#'   neurons an error is generated. If SortOnUpdate is TRUE then updating an
#'   existing neuronlist should result in a new neuronlist with ordering
#'   identical to reading all neurons from scratch.
#' @param paths Paths to neuron input files (or directory containing neurons)
#' @param pattern If paths is a directory, regex that file names must match.
#' @param neuronnames Character vector or function that specifies neuron names
#' @param df Optional data frame containing information about each neuron
#' @param OmitFailures Omit failures (when TRUE) or leave an NA value in the 
#'   list
#' @param SortOnUpdate Sort the neuronlist when update adds new neurons
#' @param nl An existing neuronlist to be updated (see details)
#' @param ... Additional arguements to passed to read.neuron methods
#' @return neuronlist object containing the neurons
#' @export
#' @seealso \code{\link{read.neuron}}
read.neurons<-function(paths, pattern=NULL, neuronnames=basename, nl=NULL,
                       df=NULL, OmitFailures=TRUE, SortOnUpdate=FALSE, ...){
  if(!is.character(paths)) stop("Expects a character vector of filenames")
  
  if(length(paths)==1 && file.info(paths)$isdir)
    paths=dir(paths,pattern=pattern,full.names=TRUE)
  
  if(is.function(neuronnames))
    nn=neuronnames(paths)
  else
    nn=neuronnames
  duplicateNames=nn[duplicated(nn)]
  if(length(duplicateNames)) {
    stop("Neurons cannot have duplicate names: ",
         paste(duplicateNames,collapse=" "))
  }
  all_names=nn
  names(paths)=nn
  # Handle updates of an existing neuronlist
  if(!is.null(nl)) {
    if(!is.neuronlist(nl)) stop("nl must be a neuronlist")
    new_neurons=setdiff(nn,names(nl))
    old_neurons_we_can_see=intersect(names(nl),nn)
    old_paths=paths[old_neurons_we_can_see]
    
    new_md5s=md5sum(old_paths)
    old_md5s=sapply(nl,"[[","InputFileMD5")
    names(old_md5s)=names(nl)
    old_md5s=old_md5s[old_neurons_we_can_see]
    stopifnot(length(old_md5s)==length(new_md5s))
    modified_neurons=old_neurons_we_can_see[new_md5s!=old_md5s]
    # now just select the paths that need to be (re)loaded
    nn=c(modified_neurons,new_neurons)
    # no paths to load => existing list is up to date
    if(!length(nn)) return(nl)
    message("There are ",length(modified_neurons)," modified neurons",
            " and ",length(new_neurons),'new neurons')
    paths=paths[nn]
  } else nl=neuronlist()
  # Look after the attached dataframe
  if(!is.null(df)){
    matching_rows=intersect(nn,rownames(df))
    if(length(matching_rows)){
      missing_rows=setdiff(nn,matching_rows)
      if(length(missing_rows))
        stop("Some neurons are not recorded in dataframe: ",
             paste(missing_rows,collapse=" "))
      missing_neurons=setdiff(matching_rows,nn)
      if(length(missing_neurons))
        warning(length(missing_neurons), 
                " rows in dataframe do not have a matching neuron.")
    } else {
      stop("Dataframe rownames do not match neuron names.")
    }
  }
  # Actually read in the neurons
  for(n in names(paths)){
    f=paths[n]
    x=try(read.neuron(f))
    if(inherits(x,'try-error')){
      if(OmitFailures) x=NULL
      else x=NA
    }
    nl[[n]]=x
  }
  if(SortOnUpdate) {
    names_missing_from_all_names=setdiff(names(nl),all_names)
    if(length(names_missing_from_all_names)){
      warning("Cannot SortOnUpdate when supplied paths do not include all neurons: ",
              paste(names_missing_from_all_names,collapse=' '))
    } else {
      # nb names_we_have will be ordered like all_names
      names_we_have=intersect(all_names,names(nl))
      # resort if required
      if(!isTRUE(all.equal(names_we_have,names(nl))))
        nl=nl[names_we_have]
    }
  }
  # nb only keep dataframe rows for neurons that were successfully read in
  attr(nl,'df')=df[names(nl),]
  nl
}

#' Set or return list of registered file formats that we can read
#' 
#' @description \code{fileformats} returns format names, a format definition 
#'   list or a table of information about the formats that match the given 
#'   filter conditions.
#' @details if a \code{format} argument is passed to \code{fileformats} it will
#'   be matched wigth partial string matching and iif a unique match exists that
#'   will be returned.
#' @inheritParams registerformat
#' @param rval Character vector choosing what kind of return value 
#'   \code{fileformats} will give.
#' @return \itemize{
#'   
#'   \item \code{fileformats} returns a character vector, matrix or list 
#'   according to the value of rval.
#'   
#'   \item \code{getformatreader} returns a list. The reader can be accessed 
#'   with \code{$read}
#'   
#'   \item \code{getformatwriter} returns a list. The writer can be accessed 
#'   with \code{$write}.}
fileformats<-function(format=NULL,ext=NULL,read=NULL,write=NULL,class=NULL, 
                        rval=c("names",'info','all')){
  currentformats=ls(envir=.fileformats)
  if(!is.null(class)){
    currentformats<-Filter(function(x) isTRUE(
      get(x,envir=.fileformats)$class%in%class), currentformats)
  }
  if(isTRUE(read)){
    currentformats<-Filter(function(x) 
      isTRUE(!is.null(get(x,envir=.fileformats)$read)), currentformats)
  }
  if(isTRUE(write)){
    currentformats<-Filter(function(x) 
      isTRUE(!is.null(get(x,envir=.fileformats)$write)), currentformats)
  }
  if(!is.null(format)) {
    m=pmatch(format, currentformats)
    if(is.na(m)) stop("Unrecognised format: ", format)
    currentformats=currentformats[m]
  } else {
    if(!is.null(ext)){
      if(substr(ext,1,1)!=".") ext=paste(".",sep="",ext)
      currentformats<-Filter(function(x) isTRUE(
        get(x,envir=.fileformats)$ext%in%ext), currentformats)
    }
  }
  rval=match.arg(rval)
  if(rval=='names'){
    currentformats
  } else if(rval=='info'){
    sapply(currentformats,function(x) {
      fx=get(x, envir=.fileformats)
      c(fx[c('ext','class')],read=!is.null(fx$read),write=!is.null(fx$write),
        magic=!is.null(fx$magic))
    })
  } else if(rval=='all') {
    mget(currentformats, envir=.fileformats)
  }
}

#' @description \code{registerformat} registers a format in the io registry
#' @export
#' @param format Character vector naming the format
#' @param ext Character vector of file extensions (including periods)
#' @param read,write Functions to read and write this format
#' @param magic Function to test whether a file is of this format
#' @param magiclen Optional integer specifying maximum number of bytes required 
#'   from file header to determine file's type.
#' @param class The S3 class for the format (character vector e.g. 'neuron')
#' @rdname fileformats
#' @examples
#' \dontrun{
#' registerformat("swc",read=read.swc,write=read.swc,magic=is.swc,magiclen=10,
#'   class='neuron')
#' }
registerformat<-function(format=NULL,ext=format,read=NULL,write=NULL,magic=NULL,
                         magiclen=NA_integer_,class=NULL){
  currentformats=ls(envir=.fileformats)
  if(format%in%currentformats)
    warning("This format has already been registered")
  if(is.null(read) && is.null(write)) 
    stop("Must provide at least one read or write function")
  
  if(substr(ext,1,1)!=".") ext=paste(".",sep='',ext)
  
  assign(format,list(ext=ext,read=read,write=write,magic=magic,magiclen=magiclen,
                     class=class),
         envir=.fileformats)
  invisible()
}

#' @description \code{getformatwriter} gets the function to read a file
#' @rdname fileformats
#' @param file Path to a file
#' @details \code{getformatreader} starts by reading a set number of bytes from 
#'   the start off the current file and then checks using file extension and 
#'   magic functions to see if it can identify the file. Presently formats are 
#'   in a queue in alphabetical order, dispatching on the first match.
#' @export
getformatreader<-function(file, class=NULL){
  formatsforclass<-fileformats(class=class)
  if(!length(formatsforclass)) return(NULL)
  
  magiclens=sapply(formatsforclass,function(f) get(f,envir=.fileformats)$magiclen)
  max_magiclen=max(c(-Inf,magiclens),na.rm=TRUE)
  if(is.finite(max_magiclen)) {
    magicbytes = readBin(file,what=raw(),n=max_magiclen)
    # check if this looks like a gzip file
    gzip_magic=as.raw(c(0x1f, 0x8b))
    if(all(magicbytes[1:2]==gzip_magic)){
      gzf=gzfile(file,open='rb')
      on.exit(close(gzf))
      magicbytes=readBin(gzf,what=raw(),n=max_magiclen)
    }
  } else magicbytes=NULL
  
  ext=tolower(sub(".*(\\.[^.]+$)","\\1",basename(file)))
  for(format in formatsforclass){
    ffs=get(format,envir=.fileformats)
    
    # check that we have a read function for this format
    if (!"read"%in%names(ffs)) next
    
    if(!is.null(ffs$magic)){
      # we have a magic function for this file, so check by candidate magic bytes
      if(ffs$magic(file, magicbytes)) return(ffs)
    } else {
      # else check by file extension
      if(ext%in%ffs$ext) return(ffs)
    }
  }
  return(NULL)
}

#' @description \code{getformatwriter} gets the function to write a file
#' @details If \code{ext=NA} then extension will not be used to query file 
#'   formats and it will be overwritten by the default extensions returned by 
#'   \code{fileformats}. If \code{ext='.someext'} \code{getformatwriter} will use the
#'   specified extension to overwrite the value returned by \code{fileformats}. 
#'   If \code{ext=NULL} and \code{file='somefilename.someext'} then \code{ext} 
#'   will be set to \code{'someext'} and that will override the value returned
#'   by \code{fileformats}.
#' @rdname fileformats
#' @export
getformatwriter<-function(format=NULL, file=NULL, ext=NULL, class=NULL){
  
  if(!is.null(file) && is.null(ext))
    ext=sub(".*(\\.[^.]+$)","\\1",basename(file))
  ext_was_set=!is.null(ext) && !is.na(ext)
  nfs=fileformats(format=format, ext=ext, class=class, rval='all')
  if(length(nfs)>1) stop("Ambiguous file format specification!")
  if(length(nfs)==0) stop("No matching writer for this file format!")
  r=nfs[[1]]
  
  if(ext_was_set) r$ext=ext
  if(!is.null(file)) r$file=sub("\\.[^.]+$",r$ext,file)
  r
}

#' Read a neuron in swc file format
#' 
#' This function should normally only be called from read.neuron and is not
#' designed for use by end users.
#' @section SWC Format: According to 
#'   \url{http://www.soton.ac.uk/~dales/morpho/morpho_doc} SWC file format has a
#'   radius not a diameter specification
#' @param f path to file
#' @param ... Additional arguments passed to \code{as.neuron()} and then on to 
#'   \code{neuron()}
read.neuron.swc<-function(f, ...){
  ColumnNames<-c("PointNo","Label","X","Y","Z","W","Parent")
  d=read.table(f, header = FALSE, sep = "", quote = "\"'", dec = ".",
               col.names=ColumnNames, check.names = TRUE, fill = FALSE,
               strip.white = TRUE, blank.lines.skip = TRUE, comment.char = "#")
  # multiply by 2 to get diam which is what I work with internally
  d$W=d$W*2
  as.neuron(d, InputFileName=f, ...)
}
