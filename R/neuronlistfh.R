# define neuronlistfh, that looks like a regular in memory neuronlist
# but is actually backed by an on disk filehash structure

#' neuronlistfh class to store multiple neurons cached on disk
#' 
#' @description \code{neuronlistfh} objects consist of a list of neuron objects
#'   along with an optional attached dataframe containing information about the
#'   neurons.
#' @details \code{neuronlistfh} objects also inherit from \code{neuronlist} and
#'   therefore any appropriate methods e.g. \code{plot3d.neuronlist} can also be
#'   used on \code{neuronlistfh} objects.
#' Presently only backing objects which extend the \code{filehash} class are
#'   supported. These include:
#' \itemize{
#' \item filehash RDS
#' \item filehash RDS2 (experimental)
#' }
#' We have implemented a simple remote access protocol which is currently only
#' implemented for the \code{RDS} format. This allows a neuronlistfh object to
#' be read from a url and downloaded to a local path. Subsequent attempts to
#' access neurons stored in this list will result in automated download of the
#' requested neuron to the local cache.
#' 
#' The \code{RDS2} format is experimental and only available at 
#' \url{https://github.com/jefferis/filehash} but is likely to be the most 
#' effective for large (>5000) collections of neurons.
#' 
#' Note that objects are stored in a filehash, which by definition does not have
#' any ordering of its elements. However neuronlist objects (like lists) do have
#' an ordering. Therefore the names of a neuronlistfh object are not necessarily
#' the same as the result of calling \code{names()} on the underlying filehash
#' object.
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
#' @description \code{neuronlistfh} constructs a neuronlistfh object from a 
#  filehash, dataframe and hashtable.
#  @export
#' @details In \code{neuronlistfh} the rownames of the dataframe 
#'   determine the ordering of the objects, not the values of \code{names()} 
#'   reported by the backing database (which does not have an intrinsic order).
#' @importFrom methods is
neuronlistfh<-function(x, df, hashtable){
  if(!is(x,'filehash'))
    stop("Unknown/supported backing db class. See ?neuronlistfh for help.")

  nlfh=hashtable

  attr(nlfh,'db')=x
  class(nlfh)=c('neuronlistfh','neuronlist',class(nlfh))
  
  if(missing(df) || is.null(df)) {
    #names(nlfh)=names(x)
  } else {
    nmissing=sum(!names(hashtable)%in%rownames(df))
    if(nmissing>0)
      stop("data.frame is missing information about ",nmissing," elements of x")
    names(nlfh)=intersect(rownames(df),names(hashtable))
    attr(nlfh,'df')=df
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
#' kcs20fh=as.neuronlistfh(kcs20,dir='/path/to/my/kcdb/data',filehash.type='RDS')
#' plot3d(subset(kcs20fh,type=='gamma'))
#' # save neuronlisfh object next to filehash backing database
#' save(kcs20fh,file='/path/to/my/kcdb/kcdb.rda')
#' 
#' # in a new session
#' load("/path/to/my/kcdb.rda")
#' plot3d(subset(kcs20fh,type=='gamma'))
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
  hashtable=sapply(x,digest)
  names(x)=hashtable
  db=filehash::dumpList(x, dbName=dir, type=dbClass)
  
  res <- neuronlistfh(db, df, hashtable)
  attr(res, 'remote') <- remote
  res
}

#' @method as.neuronlistfh filehash
#' @S3method as.neuronlistfh filehash
#' @rdname neuronlistfh
as.neuronlistfh.filehash<-function(x, df, ...)

#' @rdname neuronlistfh
as.neuronlistfh.default<-function(x, df, ...) {
  if(!inherits(x,'neuronlistfh')) class(x)<-c('neuronlistfh',class(x))

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
  hashtable=structure(as.vector(x),.Names=names(x))
  i=hashtable[i]

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
