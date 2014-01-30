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
#' \item filehash RDS, RDS2 (experimental)
#' \item stashR remoteDB and localDB objects
#' }
#' The \code{RDS2} format is experimental and only available at 
#' \url{https://github.com/jefferis/filehash} but is likely to be the most 
#' effective for large (>5000) collections of neurons. the \code{remoteDB} 
#' format has the unique feature of allowing automatic remote download of 
#' versioned neurons from a remote repository. TODO: could be interesting if
#' neuronslistfh objects could wrap multiple remote repositories.
#' 
#' Note that objects are stored in a filehash, which by definition does not have
#' any ordering of its elements. However neuronlist objects (like lists) do have
#' an ordering. Therefore the names of a neuronlistfh object are not necessarily
#' the same as the result of names on the underlying filehash object.
#' @name neuronlistfh
#' @family neuronlistfh
#' @family neuronlist
#' @import filehash
#' @seealso \code{\link[stashR]{remoteDB}, \link[filehash]{filehash-class}}
#' @examples
#' \dontrun{
#' library(stashR)
#' # set up local stashR cache of remote db containing 20 kenyon cells
#' # nb dir sets the location of the local cache and should probably be a
#' # sensible absolute path on your system.
#' kcrdb=new("remoteDB",dir='kcrdb',name='kcrdb',
#'   url='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/kcdb/')
#' kcnl=as.neuronlistfh(kcrdb)
#' # this will automatically download the neurons from the web the first time
#' # it is run
#' plot3d(kcnl)
#' }
NULL

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
#' kcs20fh=as.neuronlistfh(kcs20,dbName='/path/to/my/kcdb',filehash.type='RDS')
#' plot3d(subset(kcs20fh,type=='gamma'))
#' # save neuronlisfh object next to filehash backing database
#' save(kcs20fh,file='/path/to/my/kcdb.rda')
#' 
#' # in a new session
#' load("/path/to/my/kcdb.rda")
#' plot3d(subset(kcs20fh,type=='gamma'))
#' }
as.neuronlistfh<-function(x, df, ...)
  UseMethod("as.neuronlistfh")

#' @param dir The path to the underlying \code{filehash} database on disk
#' @param dbClass The \code{filehash} or \code{stashR} database class defaults 
#'   to \code{filehashRDS}.
#' @description \code{as.neuronlistfh.neuronlist} converts a regular neuronlist 
#'   to one backed by a filehash object with an on disk representation
#' @method as.neuronlistfh neuronlist
#' @S3method as.neuronlistfh neuronlist
#' @rdname neuronlistfh
as.neuronlistfh.neuronlist<-function(x, df=attr(x,'df'), dir=NULL,
                                     dbClass=c('RDS','RDS2',
                                               'remoteDB','localDB'), remote=NULL, ...){
  if(is.null(names(x))){
    warning("giving default names to elements of x")
    names(x)=seq(x)
  }
  dbClass=match.arg(dbClass)
  if(dbClass%in%c("RDS","RDS2")){
    db=filehash::dumpList(x, dbName=dir, type=dbClass)
  } else {
    db=new(dbClass,dir=dir, name=basename(dir), ...)
    sapply(names(x),function(n) db[[n]]=x[[n]])
  }
  res <- as.neuronlistfh(db, df)
  attr(res, 'remote') <- remote
  res
}

#' @method as.neuronlistfh filehash
#' @S3method as.neuronlistfh filehash
#' @rdname neuronlistfh
as.neuronlistfh.filehash<-function(x, df, ...) NextMethod()

#' @description \code{as.neuronlistfh.default} wraps an existing filehash/stashR
#'   object (with backing objects on disk) into a neuronlistfh
#' @S3method as.neuronlistfh default
#' @rdname neuronlistfh
#' @details In \code{as.neuronlistfh.default} the rownames of the dataframe 
#'   determine the ordering of the objects, not the values of \code{names()} 
#'   reported by the backing database (which does not have an intrinsic order).
#' @importFrom methods is
as.neuronlistfh.default<-function(x, df, ...){
  if(!is(x,'filehash'))
    stop("Unknown/supported backing db class. See ?neuronlistfh for help.")
  nlfh=as.neuronlist(vector(length=length(x)))
  attr(nlfh,'db')=x
  class(nlfh)=c('neuronlistfh',class(nlfh))
  
  if(missing(df) || is.null(df)) {
    names(nlfh)=names(x)
  } else {
    nmissing=sum(!names(x)%in%rownames(df))
    if(nmissing>0)
      stop("data.frame is missing information about ",nmissing," elements of x")
    names(nlfh)=intersect(rownames(df),names(x))
    attr(nlfh,'df')=df
  }
  nlfh
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
  if(!is.character(i)) i=names(x)[i]
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
# @param fh The filehash object that needs filling in
fillMissing <- function(missing, fh) {
  objDir <- attr(fh, 'db')@dir
  if (!file.exists(objDir)) dir.create(objDir)
  lapply(missing, function(x) download.file(url=paste0(attr(fh, 'remote'), gsub("([A-Z])", "@\\1", x, perl=T)), destfile=file.path(objDir, gsub("([A-Z])", "@\\1", x, perl=T))))
}

#' Read a local, or remote, neuronlistfh object saved to a file.
#' 
#' @param file The file path of the neuronlistfh object. Can be local, or remote (via http or ftp).
#' @param localdir If the file is to be fetched from a remote location, this is the folder in which downloaded objects will be stored.
#' @export
read.neuronlistfh <- function(file, localdir=NULL) {
  if (substr(file, 1, 7) == "http://" || substr(file, 1, 6) == "ftp://") {
    if(is.null(localdir)) stop("localdir must be specified.")
    tmpFile <- tempfile()
    download.file(url=file, destfile=tmpFile)
    objName <- load(tmpFile)
    obj <- get(objName)
    attr(obj, 'db')@dir <- localdir
    attr(obj, 'remote') <- paste0(dirname(file), '/data/')
  } else {
    objName <- load(file)
    obj <- get(objName)
  }
  obj
}
