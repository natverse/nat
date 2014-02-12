# define neuronlistfh, that looks like a regular in memory neuronlist
# but is actually backed by an on disk filehash structure

#' neuronlistfh - List of neurons loaded on demand from disk or remote website
#' 
#' @description \code{neuronlistfh} objects consist of a list of neuron objects 
#'   along with an optional attached dataframe containing information about the 
#'   neurons. In contrast to \code{neuronlist} objects the neurons are not 
#'   present in memory but are instead dynamically loaded from disk as required.
#'   \code{neuronlistfh} objects also inherit from \code{neuronlist} and 
#'   therefore any appropriate methods e.g. \code{plot3d.neuronlist} can also be
#'   used on \code{neuronlistfh} objects.
#'   
#'   \code{neuronlistfh} constructs a neuronlistfh object from a 
#'   \code{filehash}, \code{data.frame} and \code{keyfilemap}. End users will 
#'   \strong{not} typically use this function to make a \code{neuronlistfh}. 
#'   They will usually read them using \code{read.neuronlistfh} and sometimes 
#'   create them by using \code{as.neuronlistfh} on a \code{neuronlist} object.
#'   
#' @section Implementation details: neuronlistfh objects are a hybrid between 
#'   regular \code{neuronlist} objects that organise data and metadata for 
#'   collections of neurons and a backing \code{filehash} object. Instead of 
#'   keeping objects in memory, they are \emph{always} loaded from disk. 
#'   Although this sounds like it might be slow, for nearly all practical 
#'   purposes (e.g. plotting neurons) the time to read the neuron from disk is 
#'   small compared with the time to plot the neuron; the OS will cache repeated
#'   reads of the same file. The benefits in memory and startup time (<1s vs 
#'   100s for our 16,000 neuron database) are vital for collections of 1000s of 
#'   neurons e.g. for dynamic report generation using knitr or for users with 
#'   <8Gb RAM or running 32 bit R.
#'   
#'   neuronlistfh objects include: \itemize{
#'   
#'   \item{attr("keyfilemap")}{ A named character vector that determines the 
#'   ordering of objects in the neuronlist and translates keys in R to filenames
#'   on disk. For objects created by \code{as.neuronlistfh} the filenames will 
#'   be the md5 hash of the object as calculated using \code{digest}. This 
#'   design means that the same key can be used to refer to multiple distinct 
#'   objects on disk. Objects are effecitvely versioned by their contents. So if
#'   an updated neuronlistfh object is posted to a website and then fetched by a
#'   user it will result in the automated download of any updated objects to 
#'   which it refers.}
#'   
#'   \item{attr("db")}{ The backing database - typically of class 
#'   \code{filehashRDS}. This manages the loading of objects from disk.}
#'   
#'   \item{attr(x,"df")}{ The data.frame of metadata which can be used to select
#'   and plot neurons. See \code{\link{neuronlist}} for examples.}
#'   
#'   } Presently only backing objects which extend the \code{filehash} class are
#'   supported (although in theory other backing objects could be added). These 
#'   include: \itemize{
#'   
#'   \item filehash RDS
#'   
#'   \item filehash RDS2 (experimental)}
#'   
#'   We have also implemented a simple remote access protocol (currently only 
#'   for the \code{RDS} format). This allows a neuronlistfh object to be read 
#'   from a url and downloaded to a local path. Subsequent attempts to access 
#'   neurons stored in this list will result in automated download of the 
#'   requested neuron to the local cache.
#'   
#'   An alternative backend, the experimental \code{RDS2} format is supported 
#'   (available at \url{https://github.com/jefferis/filehash}). This is likely 
#'   to be the most effective for large (5,000-500,000) collections of neurons, 
#'   especially when using network filesystems (nfs, afp) which are typically 
#'   very slow at handling directory reads.
#'   
#'   Note that objects are stored in a filehash, which by definition does not 
#'   have any ordering of its elements. However neuronlist objects (like lists) 
#'   do have an ordering. Therefore the names of a neuronlistfh object are not 
#'   necessarily the same as the result of calling \code{names()} on the 
#'   underlying filehash object.
#' @name neuronlistfh
#' @family neuronlistfh
#' @family neuronlist
#' @import filehash
#' @seealso \code{\link[filehash]{filehash-class}}
#' @examples
#' \dontrun{
#' kcnl=read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds',
#' 'path/to/my/project/folder')
#' # this will automatically download the neurons from the web the first time
#' # it is run
#' plot3d(kcnl)
#' }
#' @export
#' @param db a \code{filehash} object that manages an on disk database of neuron
#'   objects. See Implementation details.
#' @param keyfilemap A named character vector in which the elements are filenames
#'   on disk (managed by the filehash object) and the names are the keys used in
#'   R to refer to the neuron objects. Note that the keyfilemap defines the order
#'   of objects in the neuronlist and will be used to reorder the dataframe if 
#'   necessary.
#' @importFrom methods is
#' @return a \code{neuronlistfh} object which is a character \code{vector} with 
#'   classes \code{neuronlistfh, neuronlist} and attributes \code{db, df}. See 
#'   Implementation details.
neuronlistfh<-function(db, df, keyfilemap){
  if(!is(db,'filehash'))
    stop("Unknown/unsupported backing db class. See ?neuronlistfh for help.")

  nlfh=structure(rep(F,length(keyfilemap)),.Names=names(keyfilemap))
  attr(nlfh,'keyfilemap')=keyfilemap
  class(nlfh)=c('neuronlistfh','neuronlist',class(nlfh))
  attr(nlfh,'db')=db
  
  if(!missing(df) && !is.null(df)) {
    nmissing=sum(!names(keyfilemap)%in%rownames(df))
    if(nmissing>0)
      stop("data.frame is missing information about ",nmissing," elements of db")
    # reorder dataframe using keyfilemap
    attr(nlfh,'df')=df[intersect(names(keyfilemap),rownames(df)),]
  }
  nlfh
}

#' @description \code{is.neuronlistfh} test if an object is a neuronlistfh
#' @param nl Object to test
#' @name neuronlistfh
#' @aliases is.neuronlistfh
is.neuronlistfh<-function(nl) {
  inherits(nl,"neuronlistfh")
}

#' @description \code{as.neuronlistfh} generic function to convert an object to
#'   neuronlistfh
#' @param x Object to convert
#' @param df Optional dataframe, where each row describes one neuron
#' @param ... Additional arguments for methods
#' @export
#' @rdname neuronlistfh
#' @examples
#' \dontrun{
#' # create neuronlistfh object backed by filehash with one file per neuron
#' # by convention we create a subfolder called data in which the objects live
#' kcs20fh=as.neuronlistfh(kcs20, dir='/path/to/my/kcdb/data')
#' plot3d(subset(kcs20fh,type=='gamma'))
#' # ... and, again by convention, save the neuronlisfh object next to filehash 
#' # backing database
#' saveRDS(kcs20fh, file='/path/to/my/kcdb/kcdb.rds')
#' 
#' # in a new session
#' read.neuronlistfh("/path/to/my/kcdb/kcdb.rds")
#' plot3d(subset(kcs20fh, type=='gamma'))
#' }
as.neuronlistfh<-function(x, df, ...)
  UseMethod("as.neuronlistfh")

#' @param dir The path to the underlying \code{filehash} database on disk
#' @param dbClass The \code{filehash} database class. Defaults to \code{RDS}.
#' @param remote The url pointing to a remote repository containing files for
#'   each neuron.
#' @description \code{as.neuronlistfh.neuronlist} converts a regular neuronlist 
#'   to one backed by a filehash object with an on disk representation
#' @method as.neuronlistfh neuronlist
#' @S3method as.neuronlistfh neuronlist
#' @importFrom digest digest
#' @rdname neuronlistfh
as.neuronlistfh.neuronlist<-function(x, df=attr(x,'df'), dir=NULL,
                                     dbClass=c('RDS','RDS2'), remote=NULL, ...){
  if(is.null(names(x))){
    warning("giving default names to elements of x")
    names(x)=seq(x)
  }
  dbClass=match.arg(dbClass)
  if(dbClass!='RDS' && !is.null(remote))
    stop("remote download only implemented for RDS class at the moment")
  # md5 by default. Should we use something else?
  keyfilemap=sapply(x,digest)
  names(x)=keyfilemap
  db=filehash::dumpList(x, dbName=dir, type=dbClass)
  
  res <- neuronlistfh(db, df, keyfilemap)
  attr(res, 'remote') <- remote
  res
}

#' convert neuronlistfh to a regular (in memory) neuronlist
#' @method as.neuronlist neuronlistfh
#' @S3method as.neuronlist neuronlistfh
#' @inheritParams as.neuronlist
as.neuronlist.neuronlistfh<-function(l, ...){
  # get the overloaded subscripting operator to do the work
  l[names(l)]
}

#' @S3method [[ neuronlistfh
"[[.neuronlistfh"<-function(x,i,...){

  # we need to translate the incoming key to the md5 hash
  # this should cover all cases (numeric, logical, names)
  i=attr(x,'keyfilemap')[i]

  if(is.null(attr(x,'remote'))){
    # no remote specified, just treat as normal
    return(attr(x,'db')[[i,...]])
  }
  # we have a remote. let's try and use it to fetch these keys
  tryCatch({
    attr(x,'db')[[i,...]]
  }, error = function(e) {
    errMsg <- e$message
    key <- substr(errMsg, regexpr("'", errMsg) + 1, nchar(errMsg) - 1)
    fillMissing(key, x)
    tryCatch({
      attr(x, 'db')[[i, ...]]
    }, error = function(e) {
      "Unable to download file."
      stop(e)
    })
  })
}

#' @S3method as.list neuronlistfh
as.list.neuronlistfh<-function(x, ...) x

#' extract a sublist from a neuronlistfh, converting to regular in memory list
#'
#' Note that if i is a numeric or logical indexing vector, it will be converted
#' internally to a vector of names by using the (sorted) names of the objects
#' in x (i.e. names(x)[i])
#' @param x A neuronlistfh object
#' @param i Indices of items to extract from neuronlistfh object
#' @param ... Additional arguments passed to neuronlistfh [] function
#' @return A new neuronlist object (i.e. in memory)
#' @export
#' @method [ neuronlistfh
"[.neuronlistfh" <- function(x,i,...) {
  if(!is.character(i)) i=names(x)[i]
  l=lapply(i,function(n) x[[n]])
  as.neuronlist(l,df=attr(x,'df')[i,])
}

# Called if some objects in the filehash object are not available locally
#
# @param missing A list of missing objects
# @param fh The neuronlistfh object that needs filling in
fillMissing <- function(missing, fh) {
  objDir <- attr(fh, 'db')@dir
  if (!file.exists(objDir)) dir.create(objDir)
  mapply(download.file, url=paste0(attr(fh, 'remote'), missing),
         destfile=file.path(objDir,missing))
}

#' Read a local, or remote, neuronlistfh object saved to a file.
#' 
#' @details When reading a remote \code{neuronlistfh} object, it is downloaded 
#'   and cached to \code{localdir}. If there is already a cached file at the 
#'   appropriate location and \code{update=TRUE} then the md5sums are checked 
#'   and the downloaded file will be copied on top of the original copy if they 
#'   are different; if \code{udpate=FALSE}, the default, then no action will be
#'   taken.
#'   
#' @param file The file path of the neuronlistfh object. Can be local, or remote
#'   (via http or ftp).
#' @param localdir If the file is to be fetched from a remote location, this is 
#'   the folder in which downloaded objects will be stored.
#' @param update Whether to update local copy of neuronlistfh (default: FALSE, 
#'   see details)
#' @param ... Extra arguments to pass to \code{download.file}.
#'   
#' @export
#' @importFrom tools md5sum
read.neuronlistfh <- function(file, localdir=NULL, update=FALSE, ...) {
  if (substr(file, 1, 7) == "http://" || substr(file, 1, 6) == "ftp://") {
    if(is.null(localdir)) stop("localdir must be specified.")
    if(!file.exists(localdir)) dir.create(localdir, recursive=TRUE)

    cached.neuronlistfh<-file.path(localdir,basename(file))
    
    if(!file.exists(cached.neuronlistfh) || update){
      tmpFile <- tempfile()
      on.exit(unlink(tmpFile))
      download.file(url=file, destfile=tmpFile, ...)
      obj <- readRDS(tmpFile)
      
      # fix paths in our new object
      attr(obj, 'db')@dir <- file.path(localdir,'data')
      attr(obj, 'remote') <- paste0(dirname(file), '/data/')
      
      # save it to disk
      saveRDS(obj,file=tmpFile)
      # and copy / replace existing copy
      if(!file.exists(cached.neuronlistfh) || isTRUE(md5sum(cached.neuronlistfh)!=md5sum(tmpFile))){
        message("Updating cached neuronlistfh: ",basename(cached.neuronlistfh))
        file.copy(tmpFile,cached.neuronlistfh)
      }
      return(obj)
    } else {
      file = cached.neuronlistfh
    }
  }
  
  obj<-readRDS(file)
  
  # fix path to filehash in object that we have read from disk just to be safe
  attr(obj, 'db')@dir <- file.path(dirname(file),'data')
  obj
}
