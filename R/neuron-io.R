#' Read a single neuron from a file
#' 
#' @details This function will handle \code{neuron} and \code{dotprops} objects 
#'   saved in R .rds or .rda format by default. Additional file formats can be 
#'   registered using \code{neuronformats}.
#' @export
#' @param f Path to file
#' @param ... additional arguments passed to format-specific readers
#' @seealso \code{\link{read.neurons}, \link{neuronformats}}
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
    ffs=getformatfuns(f,action='read')
    if(is.null(ffs)) stop("Unable to identify file type of:", f)
    n=ffs$read(f, ...)
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
    paths=dir(paths,pattern=pattern,full=TRUE)
  
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
#' @section SWC Format: According to
#'   \url{http://www.soton.ac.uk/~dales/morpho/morpho_doc} SWC file format has a
#'   radius not a diameter specification
#' @param f path to file
#' @param ... Additional arguments passed to \code{as.neuron()} and then on to
#'   \code{neuron()}
#' @seealso \code{\link{read.neuron},\link{neuron}, \link{as.neuron}}
#' @export
#' @examples
#' n=read.neuron.swc(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'))
read.neuron.swc<-function(f, ...){
  ColumnNames<-c("PointNo","Label","X","Y","Z","W","Parent")
  d=read.table(f, header = FALSE, sep = "", quote = "\"'", dec = ".",
               col.names=ColumnNames, check.names = TRUE, fill = FALSE,
               strip.white = TRUE, blank.lines.skip = TRUE, comment.char = "#")
  # multiply by 2 to get diam which is what I work with internally
  d$W=d$W*2
  as.neuron(d, InputFileName=f, ...)
}

#' read a neuron in neurolucida file format
read.neuron.neurolucida<-function(f){
  message("neurolucida reader not yet implemented!")
  NULL
}
