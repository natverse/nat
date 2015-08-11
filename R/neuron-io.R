#' Read a single neuron from a file
#' 
#' @details This function will handle \code{neuron} and \code{dotprops} objects 
#'   saved in R .rds or .rda format by default. Additional file formats can be 
#'   registered using \code{fileformats}.
#'   
#'   At the moment the following formats are supported using file readers 
#'   already included with the nat package: \itemize{
#'   
#'   \item \bold{swc} See \code{\link{read.neuron.swc}}
#'   
#'   \item \bold{neuroml} See \code{\link{read.neuron.neuroml}}
#'   
#'   \item \bold{fijitraces} See \code{\link{read.neuron.fiji}}. The file format
#'   used by the \href{http://fiji.sc/Simple_Neurite_Tracer}{Simple Neurite 
#'   Tracer} plugin of Fiji/ImageJ.
#'   
#'   \item \bold{hxlineset,hxskel} Two distinct fileformats used by Amira. 
#'   \code{hxlineset} is the generic one, \code{hxskel} is used by the 
#'   hxskeletonize extension of Schmitt and Evers (see refs).
#'   
#'   \item \bold{rda,rds} Native R cross-platform binary formats (see 
#'   \code{\link{load}, \link{readRDS}}). Note that RDS only contains a single 
#'   unnamed neuron, whereas rda contains one or more named neurons.
#'   
#'   }
#' @export
#' @param f Path to file. This can be a URL, in which case the file is
#'   downloaded to a temporary location before reading.
#' @param format The file format of the neuron. When \code{format=NULL}, the 
#'   default, \code{read.neuron} will infer the file format from the extension 
#'   or file header (aka magic) using the \code{fileformats} registry.
#' @param ... additional arguments passed to format-specific readers
#' @seealso \code{\link{read.neurons}, \link{fileformats}}
#' @references Schmitt, S. and Evers, J. F. and Duch, C. and Scholz, M. and 
#'   Obermayer, K. (2004). New methods for the computer-assisted 3-D 
#'   reconstruction of neurons from confocal image stacks. Neuroimage 4, 
#'   1283--98. 
#'   \href{http://dx.doi.org/10.1016/j.neuroimage.2004.06.047}{doi:10.1016/j.neuroimage.2004.06.047}
#'     
#' @examples
#' \dontrun{
#' # note that we override the default NeuronName field
#' n=read.neuron(system.file("tests/testthat/testdata","neuron","EBT7R.CNG.swc",package='nat'),
#'   NeuronName="EBT7R")
#' # use a function to set the NeuronName field
#' n3=read.neuron(system.file("tests/testthat/testdata","neuron","EBT7R.CNG.swc",package='nat'),
#'   NeuronName=function(x) sub("\\..*","",x))
#' # show the currently registered file formats that we can read
#' fileformats(class='neuron', read=TRUE)
#' }
read.neuron<-function(f, format=NULL, ...){
  if(grepl("^http[s]{0,1}://", f)) {
    url=f
    # download remote url to local file in tempdir
    f=file.path(tempdir(), basename(f))
    on.exit(unlink(f))
    filecontents=httr::GET(url)
    writeBin(httr::content(filecontents,type = 'raw'), con = f)
  } else url=NULL
  #if(!file.exists(f)) stop("Unable to read file: ",f)
  if(is.null(format))
    format=tolower(sub(".*\\.([^.]+$)","\\1",basename(f)))
  if(format=="rds")
    n=readRDS(f)
  else if(format=="rda"){
    objname=load(f,envir=environment())
    if(length(objname)>1) stop("More than 1 object in file:",f)
    n=get(objname,envir=environment())
  } else {
    ffs=getformatreader(f, class = 'neuron')
    if(is.null(ffs)) {
      # as a special test, check to see if this looks like an swc file
      # we don't do this by default since is.swc is a little slow
      if(is.swc(f)) n=read.neuron.swc(f, ...)
      else stop("Unable to identify file type of:", f)
    } else {
      n=match.fun(ffs$read)(f, ...)
    }
  }
  if(!is.null(url)){
    attr(n, 'url')=url
  }
  # make sure that neuron actually inherits from neuron
  # we can rely on dotprops/neuronlist objects to have the correct class
  if(is.dotprops(n) || is.neuronlist(n)) n else as.neuron(n)
}

#' Read one or more neurons from file to a neuronlist in memory
#' 
#' @details This function will cope with the same set of file formats offered by
#'   \code{read.neuron}.
#'   
#'   If the \code{paths} argument specifies a (single) directory then all files 
#'   in that directory will be read unless an optional regex pattern is also 
#'   specified. Similarly, if \code{paths} specifies a zip archive, all neurons 
#'   within the archive will be loaded.
#'   
#'   \code{neuronnames} must specify a unique set of names that will be used as 
#'   the names of the neurons in the resultant neuronlist. If \code{neuronnames}
#'   is a a function then this will be applied to the path to each neuron. The 
#'   default value is the function \code{basename} which results in each neuron 
#'   being named for the input file from which it was read.
#'   
#'   The optional dataframe (\code{df}) detailing each neuron should have 
#'   \code{rownames} that match the names of each neuron. It would also make 
#'   sense if the same key was present in a column of the data frame. If the 
#'   dataframe contains more rows than neurons, the superfluous rows are dropped
#'   with a warning. If the dataframe is missing rows for some neurons an error 
#'   is generated. If SortOnUpdate is TRUE then updating an existing neuronlist 
#'   should result in a new neuronlist with ordering identical to reading all 
#'   neurons from scratch.
#' @param paths Paths to neuron input files \emph{or} a directory containing 
#'   neurons \emph{or} a \code{\link{neuronlistfh}} object, \emph{or} a zip 
#'   archive containing multiple neurons.
#' @param pattern If paths is a directory, \link{regex} that file names must 
#'   match.
#' @param neuronnames Character vector or function that specifies neuron names. 
#'   See details.
#' @param format File format for neuron (see \code{\link{read.neuron}})
#' @param df Optional data frame containing information about each neuron
#' @param OmitFailures Omit failures (when TRUE) or leave an NA value in the 
#'   list
#' @param SortOnUpdate When \code{nl!=NULL} the resultant neuronlist will be 
#'   sorted so that neurons are ordered according to the value of the 
#'   \code{paths} argument.
#' @param nl An existing neuronlist to be updated (see details)
#' @param ... Additional arguements to passed to read.neuron methods
#' @return \code{\link{neuronlist}} object containing the neurons
#' @export
#' @seealso \code{\link{read.neuron}}
#' @family neuronlist
#' @examples
#' \dontrun{
#' ## Read C. elegans neurons from OpenWorm github repository
#' vds=paste0("VD", 1:13)
#' vdurls=paste0("https://raw.githubusercontent.com/openworm/CElegansNeuroML/",
#'   "103d500e066125688aa7ac5eac7e9b2bb4490561/CElegans/generatedNeuroML/",vds,
#'   ".morph.xml")
#' vdnl=read.neurons(vdurls, neuronnames=vds)
#' plot3d(vdnl)
#' 
#' ## The same, but this time add some metadata to neuronlist
#' # fetch table of worm neurons from wormbase
#' library(rvest)
#' nlurl="http://wormatlas.org/neurons/Individual%20Neurons/Neuronframeset.html"
#' wormneurons = html_table(html(nlurl), fill=TRUE)[[4]]
#' vddf=subset(wormneurons, Neuron%in%vds)
#' rownames(vddf)=vddf$Neuron
#' # attach metadata to neuronlist
#' vdnl=read.neurons(vdurls, neuronnames=vds, df=vddf)
#' # use metadata to plot a subset of neurons
#' clear3d()
#' plot3d(vdnl, grepl("P[1-6].app", Lineage))
#' }
read.neurons<-function(paths, pattern=NULL, neuronnames=basename, format=NULL,
                       nl=NULL, df=NULL, OmitFailures=TRUE, SortOnUpdate=FALSE,
                       ...){
  if(length(paths) == 1 && grepl("\\.zip$", paths)) {
    neurons_dir <- file.path(tempfile(pattern = "user_neurons"))
    on.exit(unlink(neurons_dir, recursive=TRUE))
    unzip(paths, exdir=neurons_dir)
    paths=dir(neurons_dir, full.names = TRUE, recursive=TRUE)
  }
  else if(is.neuronlistfh(paths)){
    if(!inherits(attr(paths,'db'),'filehashRDS'))
      stop("read.neurons only supports reading neuronlistfh with an RDS format filehash")
    nlfh=paths
    dbdir=attr(nlfh,'db')@dir
    kfm=attr(nlfh,'keyfilemap')
    paths=structure(file.path(dbdir,kfm),.Names=names(kfm))
    if(OmitFailures) {
      fep=file.exists(paths)
      if(!all(fep)) 
        warning("There are ", sum(fep), " missing neurons!")
      paths=paths[fep]
    }
    neuronnames=names(paths)
    df=attr(nlfh,'df')
    format='rds'
  }
  
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
            " and ",length(new_neurons),' new neurons')
    paths=paths[nn]
  } else nl=neuronlist()
  # Actually read in the neurons, making sure that warnings/errors are thrown
  # immediately so that we can tell which neuron generated them
  ow=options(warn=1)
  on.exit(options(ow), add = TRUE)
  for(n in names(paths)){
    f=unname(paths[n])
    x=withCallingHandlers(try(read.neuron(f, format=format, ...)),
                          warning = function(w) message("While reading file: ",f),
                          error=function(e) message("While reading file: ",f))
    if(inherits(x,'try-error')){
      if(OmitFailures) x=NULL
      else x=NA
    }
    if(is.neuronlist(x)) {
      if(length(paths)>1)
        stop("It is not possible to load multiple neuronlists or a mix of",
             " neurons and neuronlists!\nPlease specfiy a single file or",
             " use a pattern to exclude files you did not mean to read.")
      nl=x
      break
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
  # Look after the attached dataframe
  if(!is.null(df)){
    data.frame(nl)=df
  }
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
#'   with \code{$read} and the format can be acessed by \code{$format}.
#'   
#'   \item \code{getformatwriter} returns a list. The writer can be accessed 
#'   with \code{$write}.}
#' @export
#' @examples
#' # information about the currently registered file formats
#' fileformats(rval='info')
fileformats<-function(format=NULL,ext=NULL,read=NULL,write=NULL,class=NULL, 
                        rval=c("names",'info','all')){
  currentformats<-ls(envir=.fileformats)
  
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
    if(is.na(m)) stop("No format available to meet this specification: ", format)
    currentformats=currentformats[m]
  } else {
    if(!is.null(ext) && !is.na(ext)){
      if(substr(ext,1,1)!=".") ext=paste(".",sep="",ext)
      currentformats<-Filter(function(x) isTRUE(
        ext%in%get(x,envir=.fileformats)$ext), currentformats)
    }
  }
  rval=match.arg(rval)
  if(rval=='names'){
    currentformats
  } else if(rval=='info'){
    t(sapply(currentformats,function(x) {
      fx=get(x, envir=.fileformats)
      c(fx[c('ext','class')],read=!is.null(fx$read),write=!is.null(fx$write),
        magic=!is.null(fx$magic))
    }))
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
  
  ext=sub("^([^.])",".\\1",ext)
  
  assign(format,list(ext=ext,read=read,write=write,magic=magic,magiclen=magiclen,
                     class=class),
         envir=.fileformats)
  invisible()
}

#' @description \code{getformatreader} gets the function to read a file
#' @rdname fileformats
#' @param file Path to a file
#' @details \code{getformatreader} starts by reading a set number of bytes from 
#'   the start off the current file and then checks using file extension and 
#'   magic functions to see if it can identify the file. Presently formats are 
#'   in a queue in alphabetical order, dispatching on the first match.
#' @export
#' @examples
#' swc=tempfile(fileext = '.swc')
#' write.neuron(Cell07PNs[[1]], swc)
#' stopifnot(isTRUE(getformatreader(swc)$format=='swc'))
#' unlink(swc)
getformatreader<-function(file, class=NULL){
  formatsforclass<-fileformats(class=class, read = TRUE)
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
    ffs$format=format
    
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
#' @section getformatwriter output file: If \code{getformatwriter} is passed a 
#'   \code{file} argument, it will be processed based on the registered 
#'   fileformats information and the \code{ext} argument to give a final output 
#'   path in the \code{$file} element of the returned \code{list}.
#'   
#'   If \code{ext='.someext'} \code{getformatwriter} will use the specified 
#'   extension to overwrite the default value returned by \code{fileformats}.
#'   
#'   If \code{ext=NULL}, the default, and \code{file='somefilename.someext'}
#'   then \code{file} will be untouched and \code{ext} will be set to
#'   \code{'someext'} (overriding the value returned by \code{fileformats}).
#'   
#'   If \code{file='somefile_without_extension'} then the suppplied or 
#'   calculated extension will be appended to \code{file}.
#'   
#'   If \code{ext=NA} then the input \code{file} name will not be touched (even 
#'   if it has no extension at all).
#'   
#'   Note that if \code{ext=NULL} or \code{ext=NA}, then only the specified 
#'   format or, failing that, the \code{file} extension will be used to query 
#'   the fileformats database for a match.
#'   
#'   See \code{\link{write.neuron}} for code to make this discussion more 
#'   concrete.
#' @rdname fileformats
#' @export
#' @seealso \code{\link{write.neuron}}
getformatwriter<-function(format=NULL, file=NULL, ext=NULL, class=NULL){
  
  if(!is.null(file) && is.null(ext)){
    incomingext=tools::file_ext(file)
    if(nzchar(incomingext)) ext=paste0(".", incomingext)
  }
  ext_was_set=!is.null(ext) && !is.na(ext)
  
  nfs=fileformats(format=format, ext=ext, class=class, rval='all', write=TRUE)
  if(length(nfs)>1) stop("Ambiguous file format specification!")
  if(length(nfs)==0) stop("No matching writer for this file format!")
  r=nfs[[1]]
  
  if(ext_was_set) r$ext=ext
  # Process file name if one was supplied
  if(!is.null(file)) {
    r$file=if(length(ext) && is.na(ext)) {
      # ext=NA, don't touch file name!
      file
    } else if(nzchar(tools::file_ext(file))){
      # the file we were given has an extension, replace it
      sub("\\.[^.]+$", r$ext[1], file)
    } else {
      # the input file did not have an extension
      # append if ext was anything other than NA
      paste0(file, r$ext[1])
    }
  }
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
#' @seealso \code{\link{is.swc}}
read.neuron.swc<-function(f, ...){
  ColumnNames<-c("PointNo","Label","X","Y","Z","W","Parent")
  d=read.table(f, header = FALSE, sep = "", quote = "\"'", dec = ".",
               col.names=ColumnNames, check.names = TRUE, fill = FALSE,
               strip.white = TRUE, blank.lines.skip = TRUE, comment.char = "#",
               colClasses=c("integer",'integer','numeric','numeric','numeric','numeric','integer'))
  # multiply by 2 to get diam which is what I work with internally
  d$W=d$W*2
  as.neuron(d, InputFileName=f, ...)
}

#' Test if a file is an SWC format neuron
#' 
#' @details Note that this test is somewhat expensive compared with the other 
#'   file tests since SWC files do not have a consistent magic value. It
#'   therefore often has to read and parse the first few lines of the file in 
#'   order to determine whether they are consistent with the SWC format.
#' @param f Path to one or more files
#' @inheritParams is.nrrd
#' @return logical value
#' @seealso \code{\link{read.neuron}}
#' @export
is.swc<-function(f, TrustSuffix=TRUE) {
  if(length(f)>1) return(sapply(f, is.swc, TrustSuffix))
  if(TrustSuffix && tools::file_ext(f)=='.swc') return(TRUE)
  
  first_char=readChar(f, nchars = 1, useBytes = TRUE)
  
  # must start with a number or a comment character
  if(!grepl(first_char, "#0123456789", fixed = TRUE, useBytes = T))
    return(FALSE)
  
  # try reading a line, treating any warnings as errors
  first_line=try(withCallingHandlers(
    read.table(f, nrows = 1, col.names = 
                 c("PointNo","Label","X","Y","Z","W","Parent")),
    warning=function(w) stop(w) ),
    silent = TRUE)
  if(inherits(first_line, 'try-error')) return(FALSE)
  
  # these columns must be integer
  if(!all(sapply(first_line[c("PointNo", "Label", "Parent")], is.integer)))
    return(FALSE)
  # final check these columns must be numeric (double or integer)
  all(sapply(first_line[c("X", "Y", "Z", "W")], is.numeric))
}

#' Write out a neuron in any of the file formats we know about
#' 
#' If file is not specified the neuron's InputFileName field will be checked 
#' (for a dotprops object it will be the \code{'file'} attribute). If this is 
#' missing there will be an error. If dir is specified it will be combined with 
#' basename(file). If file is specified but format is not, it will be inferred 
#' from file's extension.
#' 
#' @details Note that if \code{file} does not have an extension then the default
#'   extension for the specified \code{format} will be appended. This behaviour
#'   can be suppressed by setting \code{ext=NA}.
#'   
#' @param n A neuron
#' @param file Path to output file
#' @param dir Path to directory (this will replace dirname(file) if specified)
#' @param format Unique abbreviation of one of the registered file formats for 
#'   neurons including 'swc', 'hxlineset', 'hxskel'
#' @param ext Will replace the default extension for the filetype and should 
#'   include the period eg \code{ext='.amiramesh'} or \code{ext='_reg.swc'}. The
#'   special value of ext=NA will prevent the extension from being changed or 
#'   added e.g. if the desired file name does not have an extension.
#' @param Force Whether to overwrite an existing file
#' @param MakeDir Whether to create directory implied by \code{file} argument.
#' @param ... Additional arguments passed to selected writer function
#' @return return value
#' @export
#' @seealso \code{\link{fileformats}, \link{saveRDS}}
#' @examples
#' # show the currently registered file formats that we can write
#' fileformats(class='neuron', write=TRUE)
#' \dontrun{
#' write.neuron(Cell07PNs[[1]], file='myneuron.swc')
#' # writes out "myneuron.swc" in SWC format
#' write.neuron(Cell07PNs[[1]], format = 'hxlineset', file='myneuron.amiramesh')
#' # writes out "myneuron.amiramesh" in Amira hxlineset format
#' write.neuron(Cell07PNs[[1]], format = 'hxlineset', file='myneuron')
#' # writes out "myneuron.am" in Amira hxlineset format
#' }
write.neuron<-function(n, file=NULL, dir=NULL, format=NULL, ext=NULL, 
                       Force=FALSE, MakeDir=TRUE, ...){
  if(is.dotprops(n)){
    # we only know how to save dotprops objects in R's internal format
    format=if(is.null(format)) 'rds' else match.arg(format, c("swc", "rds"))
    if(is.null(file)) {
      file=basename(attr(n,"file"))
      if(is.null(file))
        stop("No file specified and dotprops obj does not have a file attribute")
      # trim off the suffix unless we have specified ext=NA 
      if(!(length(ext) && is.na(ext))) file=tools::file_path_sans_ext(file)
    }
  }
  if(is.null(file)){
    # no file was specified - use the one embedded in neuron
    file=basename(n$InputFileName)
    if(is.null(file))
      stop("No file specified and neuron does not have an InputFileName")
    # trim off the suffix if ext!=NA
    if(!(length(ext) && is.na(ext)))
      file=tools::file_path_sans_ext(file)
  }
  fw=getformatwriter(format=format, file=file, ext=ext, class='neuron')
  file=fw$file
  if(!is.null(dir)) file=file.path(dir,basename(file))
  
  # Now check that we can write to the location that we have chosen
  if(!Force && file.exists(file)){
    warning(file," already exists; use Force=T to overwrite")
    return(NA_character_)
  }
  if(!file.exists(dirname(file))){
    if(MakeDir){
      if(!dir.create(dirname(file)))
        stop("Unable to create ",dirname(file))
    } else {
      stop(dirname(file)," does not exist; use MakeDir=T to overwrite")
    }
  }
  if(!file.create(file)){
    stop("Unable to write to file ",file)
  }
  
  # OK all fine, so let's write
  match.fun(fw$write)(n, file=file, ...)
  invisible(file)
}

# write neuron to SWC file
write.neuron.swc<-function(x, file, ...){
  if(is.dotprops(x)) {
    return(write.dotprops.swc(x, file, ...))
  }
  # assume we have a neuron
  our_col_names<-c("PointNo","Label","X","Y","Z","W","Parent")
  if(!all(our_col_names%in%colnames(x$d))) stop("Some columns are missing!")
  df=x$d[,our_col_names]
  colnames(df)[colnames(df)=="W"]="Radius"
  
  # nb neurolucida seems to use diam, but swc uses radius
  df$Radius=df$Radius/2
  writeLines(c("# SWC format file",
               "# based on specifications at http://research.mssm.edu/cnic/swc.html"),
             con=file)
  cat("# Created by nat::write.neuron.swc\n", file=file, append=TRUE)  
  cat("#", colnames(df), "\n", file=file, append=TRUE)
  write.table(df, file, col.names=F, row.names=F, append=TRUE, ...)
}

write.dotprops.swc<-function(x, file, ...) {
  df=dotprops2swc(x, ...)
  writeLines(c("# SWC dotprops format", "# Created by nat::write.dotprops.swc",
               "# see http://research.mssm.edu/cnic/swc.html"),
             con=file)
  cat("#", colnames(df), "\n", file=file, append=TRUE)
  write.table(df, file, col.names=F, row.names=F, append=TRUE)
}


#' Write neurons from a neuronlist object to individual files, or a zip archive
#' 
#' @details See \code{\link{write.neuron}} for details of how to specify the 
#'   file format/extension/name of the output files and how to establish what 
#'   output file formats are available. A zip archive of files can be written by
#'   specifying a value of \code{dir} that ends in \code{.zip}.
#' @param nl neuronlist object
#' @param dir directory to write neurons, or path to zip archive (see Details).
#' @inheritParams write.neuron
#' @param subdir String naming field in neuron that specifies a subdirectory OR 
#'   expression to evaluate in the context of neuronlist's df attribute
#' @param INDICES Character vector of the names of a subset of neurons in 
#'   neuronlist to write.
#' @param files Character vector or expression specifying output filenames. See 
#'   examples and \code{\link{write.neuron}} for details.
#' @param ... Additional arguments passed to \code{\link{write.neuron}}
#' @inheritParams write.neuron
#' @return the path to the output file(s), absolute when this is a zip file.
#' @author jefferis
#' @export
#' @seealso \code{\link{write.neuron}}
#' @family neuronlist
#' @examples
#' \dontrun{
#' # write some neurons in swc format
#' write.neurons(Cell07PNs, dir="testwn", format='swc')
#' # write some neurons in Amira hxlineset format
#' write.neurons(Cell07PNs, dir="testwn", format='hxlineset')
#' 
#' # organise new files in directory hierarchy by glomerulus and Scored.By field
#' write.neurons(Cell07PNs,dir="testwn",
#'   subdir=file.path(Glomerulus,Scored.By),format='hxlineset')
#' # ensure that the neurons are named according to neuronlist names
#' write.neurons(Cell07PNs, dir="testwn", files=names(Cell07PNs),
#'   subdir=file.path(Glomerulus,Scored.By),format='hxlineset')
#' # only write a subset
#' write.neurons(subset(Cell07PNs, Scored.By="ACH"),dir="testwn2",
#'   subdir=Glomerulus,format='hxlineset')
#' # The same, but likely faster for big neuronlists
#' write.neurons(Cell07PNs, dir="testwn3",
#'   INDICES=subset(Cell07PNs,Scored.By="ACH",rval='names'),
#'   subdir=Glomerulus,format='hxlineset')
#' # set file name explicitly using a field in data.frame
#' write.neurons(subset(Cell07PNs, Scored.By="ACH"),dir="testwn4",
#'   subdir=Glomerulus, files=paste0(ID,'.am'), format='hxlineset')
#' }
write.neurons<-function(nl, dir, format=NULL, subdir=NULL, INDICES=names(nl), 
                        files=NULL, Force=FALSE, ...){
  if(grepl("\\.zip", dir)) {
    zip_file=dir
    # check if file exists (and we want to overwrite)
    if(file.exists(zip_file)){
      if(!Force)
        stop("Zip file: ", zip_file, "already exists")
      unlink(zip_file)
    }
    # Get absolute path of parent dir
    zip_dir=tools::file_path_as_absolute(dirname(zip_file))
    # ... and use that to construct absolute path to output zip
    zip_file=file.path(zip_dir, basename(zip_file))
    dir <- file.path(tempfile("user_neurons"))
  } else {
    zip_file=NULL
  }
  if(!file.exists(dir)) dir.create(dir)
  df=attr(nl,'df')
  # Construct subdirectory structure based on variables in attached data.frame
  ee=substitute(subdir)
  subdirs=NULL
  if(!is.null(ee) && !is.character(ee)){
    if(!is.null(df)) df=df[INDICES,]
    subdirs=file.path(dir, eval(ee, df, parent.frame()))
    names(subdirs)=INDICES
  }
  ff=substitute(files)
  if(!is.null(ff)){
    if(!is.character(ff))
      files=eval(ff, df, parent.frame())
    if(is.null(names(files))) names(files)=INDICES
  }
  written=structure(rep("",length(INDICES)), .Names = INDICES)
  for(nn in INDICES){
    n=nl[[nn]]
    thisdir=dir
    if(is.null(subdirs)){
      if(!is.null(subdir)){
        propval=n[[subdir]]
        if(!is.null(propval)) thisdir=file.path(dir, propval)
      }
    } else {
      thisdir=subdirs[nn]
    }
    if(!file.exists(thisdir)) dir.create(thisdir, recursive=TRUE)
    written[nn]=write.neuron(n, dir=thisdir, file = files[nn], format=format, Force=Force, ...)
  }
  if(!is.null(zip_file)) {
    owd=setwd(dir)
    on.exit(setwd(owd))
    zip(zip_file, files=dir(dir, recursive = TRUE))
    unlink(dir, recursive=TRUE)
    written<-zip_file
  }
  invisible(written)
}
