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
#' @section Modifying neuronlistfh objects: The recommended way to do this is by
#'   using the \code{c.neuronlistfh} method to append one or more neuronlists to
#'   a neuronlistfh object. This ensures that the attached metadata for each
#'   data.frame is handled properly. Use as \code{nlfh <- c(nlfh, nl2)}. If you
#'   want to combine two \code{neuronlistfh} objects, it may make sense to
#'   choose the bigger one as the first-listed argument to which additional
#'   neurons are appended.
#'
#'   There is also low-level and quite basic support for modifying neuronlistfh
#'   objects using the \code{[[} operator. There are two modes depending on the
#'   nature of the index in the assignment operation
#'   \code{nlfh[[index]]<-neuron}:
#'
#'   \itemize{
#'
#'   \item numeric index \emph{for replacement of items only}
#'
#'   \item character index \emph{for replacement \bold{or} addition of items}
#'
#'   }
#'
#'   This distinction is because there must be a character key provided to name
#'   the neuron when a new one is being added, whereas an existing element can
#'   be referenced by position (i.e. the numeric index). Unfortunately the end
#'   user is responsible for manually modifying the attached data.frame when new
#'   neurons are added. Doing \code{nlfh[[index]]<-neuron} will do the
#'   equivalent of \code{attr(nlfh, 'df')[i, ]=NA} i.e. add a row containing NA
#'   values.
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
#'   neuronlistfh objects include: \describe{
#'
#'   \item{\code{attr("keyfilemap")}}{ A named character vector that determines
#'   the ordering of objects in the neuronlist and translates keys in R to
#'   filenames on disk. For objects created by \code{as.neuronlistfh} the
#'   filenames will be the md5 hash of the object as calculated using
#'   \code{digest}. This design means that the same key can be used to refer to
#'   multiple distinct objects on disk. Objects are effectively versioned by
#'   their contents. So if an updated neuronlistfh object is posted to a website
#'   and then fetched by a user it will result in the automated download of any
#'   updated objects to which it refers.}
#'
#'   \item{\code{attr("db")}}{ The backing database - typically of class
#'   \code{filehashRDS}. This manages the loading of objects from disk.}
#'
#'   \item{\code{attr(x,"df")}}{ The data.frame of metadata which can be used to
#'   select and plot neurons. See \code{\link{neuronlist}} for examples.}
#'
#'   \item{\code{attr(x,"hashmap")}}{ (Optional) a hashed environment which can
#'   be used for rapid lookup using key names (rather than numeric/logical
#'   indices). There is a space potential to pay for this redundant lookup
#'   method, but it is normally worth while given that the dataframe object is
#'   typically considerably larger. To give some numbers, the additional
#'   environment might occupy ~ 1% of a 16,000 object neuronlistfh object and
#'   reduce mean lookup time from 0.5 ms to 1us. Having located the object, on
#'   my machine it can take as little as 0.1ms to load from disk, so these
#'   savings are relevant.}
#'
#'   }
#'
#'   Presently only backing objects which extend the \code{filehash} class are
#'   supported (although in theory other backing objects could be added). These
#'   include: \itemize{
#'
#'   \item filehash RDS
#'
#'   \item filehash RDS2 (experimental)
#'
#'   \item filehash DB1 (experimental)
#'
#'   }
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
#'   especially when using network filesystems (\code{NFS}, \code{AFP}) which
#'   are typically very slow at listing large directories.
#'
#'   Finally the DB1 backend keeps the data in a single monolithic file on disk.
#'   This may work better when there are many small neurons (think >10,000 files
#'   occupying only a few GB) on NFS network file systems or Google Drive,
#'   neither of which are keen on having many files especially in the same
#'   folder. It does not allow updates from a remote location. See
#'   \code{\link{filehashDB1-class}} for more details.
#'
#'   Note that objects are stored in a filehash, which by definition does not
#'   have any ordering of its elements. However neuronlist objects (like lists)
#'   do have an ordering. Therefore the names of a neuronlistfh object are not
#'   necessarily the same as the result of calling \code{names()} on the
#'   underlying filehash object.
#' @name neuronlistfh
#' @family neuronlistfh
#' @family neuronlist
#' @seealso \code{\link[filehash]{filehash-class}}
#' @examples
#' \dontrun{
#' kcnl=read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds',
#' 'path/to/my/project/folder')
#' # this will automatically download the neurons from the web the first time
#' # it is run
#' plot3d(kcnl)
#' 
#' kcfh <- as.neuronlistfh(kcs20[1:18])
#' # add more neurons
#' kcfh <- c(kcfh, kcs20[19], kcs20[20])
#' # convert back to regular (in memory) neuronlist
#' all.equal(as.neuronlist(kcfh), kcs20)
#' }
#' @export
#' @param db a \code{filehash} object that manages an on disk database of neuron
#'   objects. See Implementation details.
#' @param keyfilemap A named character vector in which the elements are
#'   filenames on disk (managed by the filehash object) and the names are the
#'   keys used in R to refer to the neuron objects. Note that the keyfilemap
#'   defines the order of objects in the neuronlist and will be used to reorder
#'   the dataframe if necessary.
#' @param hashmap A logical indicating whether to add a hashed environment for
#'   rapid object lookup by name or an integer or an integer defining a
#'   threshold number of objects when this will happen (see Implementation
#'   details).
#' @importClassesFrom filehash filehash filehashRDS
#' @importMethodsFrom filehash [[
#' @importFrom methods is
#' @return a \code{neuronlistfh} object which is a character \code{vector} with
#'   classes \code{neuronlistfh, neuronlist} and attributes \code{db, df}. See
#'   Implementation details.
neuronlistfh<-function(db, df, keyfilemap, hashmap=1000L){
  if(!is(db,'filehash'))
    stop("Unknown/unsupported backing db class. See ?neuronlistfh for help.")

  if(!is.character(keyfilemap) || is.null(names(keyfilemap)) || 
       any(duplicated(names(keyfilemap)))){
    stop("keyfilemap must have as many unique names as elements")
  }
  
  nlfh=structure(rep(F,length(keyfilemap)),.Names=names(keyfilemap))
  attr(nlfh,'keyfilemap')=keyfilemap
  class(nlfh)=c('neuronlistfh','neuronlist',class(nlfh))
  attr(nlfh,'db')=db
  if(is.numeric(hashmap)) hashmap = length(keyfilemap)>=hashmap
  
  if(hashmap){
    attr(nlfh,'hashmap')=list2env(as.list(keyfilemap))
  }
  
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
#' @param ... Additional arguments for methods, eventually passed to
#'   \code{neuronlistfh()} constructor.
#' @export
#' @rdname neuronlistfh
#' @examples
#' \dontrun{
#' # create neuronlistfh object backed by filehash with one file per neuron
#' # by convention we create a subfolder called data in which the objects live
#' kcs20fh=as.neuronlistfh(kcs20, dbdir='/path/to/my/kcdb/data')
#' plot3d(subset(kcs20fh,type=='gamma'))
#' # ... and, again by convention, save the neuronlisfh object next to filehash 
#' # backing database
#' write.neuronlistfh(kcs20fh, file='/path/to/my/kcdb/kcdb.rds')
#' 
#' # in a new session
#' read.neuronlistfh("/path/to/my/kcdb/kcdb.rds")
#' plot3d(subset(kcs20fh, type=='gamma'))
#' 
#' # using the DB1 backing store (a single file on disk for all objects)
#' kcs20fh=as.neuronlistfh(kcs20, dbdir='/path/to/my/kcdb/kcs20fh')
#' # store metadata on disk
#' write.neuronlistfh(kcs20fh, file='/path/to/my/kcdb/kcs20fh.rds')
#' # read in again in a new session. You will need these two files
#' # kcs20fh kcs20fh.rds
#' kcs20fh2 <- read.neuronlistfh("/path/to/my/kcdb/kcs20fh.rds")
#' }
as.neuronlistfh<-function(x, df, ...)
  UseMethod("as.neuronlistfh")

#' @param dbdir The path to the underlying \code{filehash} database on disk. For
#'   RDS formats, by convention this should be a path whose final element is
#'   'data' which will be turned into a directory. For DB1 format it specifies
#'   a single file to which objects will be written.
#' @param dbClass The \code{filehash} database class. Defaults to \code{RDS}.
#' @param remote The url pointing to a remote repository containing files for
#'   each neuron.
#' @param WriteObjects Whether to write objects to disk. Missing implies that
#'   existing objects will not be overwritten. Default \code{"yes"}.
#' @description \code{as.neuronlistfh.neuronlist} converts a regular neuronlist
#'   to one backed by a filehash object with an on disk representation
#' @export
#' @importFrom digest digest
#' @rdname neuronlistfh
as.neuronlistfh.neuronlist<-function(x, df=attr(x,'df'), dbdir=NULL,
                                     dbClass=c('RDS','RDS2', 'DB1'), remote=NULL, 
                                     WriteObjects=c("yes",'no','missing'), ...){
  if(is.null(names(x))){
    warning("giving default names to elements of x")
    names(x)=seq(x)
  }
  WriteObjects=match.arg(WriteObjects)
  dbClass=match.arg(dbClass)
  if(WriteObjects!='yes' && dbClass!='RDS')
    stop("Must always write objects when dbClass!='RDS'")
  if(dbClass!='RDS' && !is.null(remote))
    stop("remote download only implemented for RDS class at the moment")
  
  if(dbClass == "DB1") {
    if(file.exists(dbdir) && isTRUE(file.info(dbdir)$isdir))
      stop("For DB1 format, dbdir must specify the single file containing data!")
  } else {
    if(file.exists(dbdir) && !isTRUE(file.info(dbdir)$isdir))
      stop("For RDS formats, dbdir must specify a folder to which data will be written!")
  }
  
  # md5 by default. Should we use something else?
  keyfilemap=sapply(x,digest)
  names(x)=keyfilemap
  if(WriteObjects=='yes'){
    db=filehash::dumpList(x, dbName=dbdir, type=dbClass)
  } else {
    if(!filehash::dbCreate(dbdir)) stop("Error creating database at location: ",dbdir)
    db=filehash::dbInit(dbdir, type=dbClass)
    if(WriteObjects=='missing') {
      # figure out which objects we need to dump
      objects_present=dir(dbdir)
      objects_missing=setdiff(keyfilemap,objects_present)
      if(length(objects_missing))
        db=filehash::dumpList(x[objects_missing], dbName=dbdir, type=dbClass)
    }
  }
  
  res <- neuronlistfh(db, df, keyfilemap, ...)
  attr(res, 'remote') <- remote
  res
}

#' convert neuronlistfh to a regular (in memory) neuronlist
#' @export
#' @inheritParams as.neuronlist
as.neuronlist.neuronlistfh<-function(l, ...){
  # get the overloaded subscripting operator to do the work
  l[names(l)]
}

#' @export
"[[.neuronlistfh"<-function(x,i,...){

  # we need to translate the incoming key to the md5 hash
  # if a hashmap is available, that will be faster for lookup by names
  if(is.character(i) && !is.null(hm<-attr(x,'hashmap'))){
    i = mget(i,envir=hm, inherits=FALSE, ifnotfound = list(NA))[[1]]
  } else i = attr(x,'keyfilemap')[i]

  if(is.na(i))
    return(NULL)
  
  if(is.null(attr(x,'remote'))){
    # no remote specified, just treat as normal
    return(attr(x,'db')[[i,...]])
  }
  
  tryCatch({
    attr(x, 'db')[[i, ...]]
  }, error = function(e) {
    fillMissing(i, x)
    tryCatch({
      attr(x, 'db')[[i, ...]]
    }, error = function(e) {
      "Unable to download file."
      stop(e)
    })
  })
}

#' @export
"[[<-.neuronlistfh" <- function (x, i, j, ..., value) {
  hash=digest(value)
  append=FALSE
  if(is.numeric(i)) {
    if(i<1 || i > length(x))
      stop("i must be between 1 and ", length(x), "To append using a string index.")
  } else {
    if(!is.character(i)) stop("i must be a numeric or character index!")
    append = !(i %in% names(x))
  }
  attr(x, "keyfilemap")[i]=hash
  filehash::dbInsert(attr(x, 'db'), hash, value)
  # add another row to dataframe
  if(append){
    # x is a dummy logical vector, key thing is that this sets the name of x[i]
    x[i]=FALSE
    attr(x, 'df')[i, ]=NA
  } else {
    # nothing to do
  }
  x
}

#' @export
#' @rdname neuronlistfh
#' @description \code{c.neuronlistfh} adds additional neurons from one or more
#'   neuronlist objects to a \code{neuronlistfh} object.
#' @param recursive currently ignored
c.neuronlistfh<-function(..., recursive = FALSE){
  args=list(...)
  # making df also does some safety checks, so do this first
  new.df=merge_nl_dataframes(args)
  x=args[[1]]
  for(nl in args[-1]) {
    for(nn in names(nl)) {
      # insert neurons one at a time
      x[[nn]]=nl[[nn]]
    }
  }
  # NB permute the merged data frame to ensure that it matches
  data.frame(x)=new.df[names(x),,drop=FALSE]
  x
}


#' @export
as.list.neuronlistfh<-function(x, ...) x

#' Extract from neuronlistfh object or its attached data.frame
#' 
#' @description \code{[.neuronlistfh} extracts either a sublist from a 
#'   neuronlistfh (converting it to a regular in memory list in the process) 
#'   \emph{or} its attached data.frame.
#'   
#' @details Note that if i is a numeric or logical indexing vector, it will be 
#'   converted internally to a vector of names by using the (sorted) names of 
#'   the objects in x (i.e. names(x)[i])
#' @param x A neuronlistfh object
#' @param i,j elements to extract or replace. Numeric, logical  or character or,
#'   for the [ get method, empty. See details and the help for 
#'   \code{\link{[.data.frame}}.
#' @inheritParams base::`[.data.frame`
#' @return A new in-memory \code{neuronlist} or when using two subscripts, a 
#'   \code{data.frame} - see examples.
#' @export
#' @family neuronlistfh
#' @seealso \code{\link{neuronlistfh}}, \code{\link{[.neuronlist}}, 
#'   \code{\link{[.data.frame}}, \code{\link{[<-.data.frame}},
#' @examples 
#' # make a test neuronlistfh backed by a temporary folder on disk
#' tf=tempfile('kcs20fh')
#' kcs20fh<-as.neuronlistfh(kcs20, dbdir=tf)
#' 
#' # get first neurons as an in memory neuronlist
#' class(kcs20fh[1:3])
#' 
#' # extract attached data.frame
#' str(kcs20fh[,])
#' # or part of the data.frame
#' str(kcs20fh[1:2,1:3])
#' 
#' # data.frame assignment (this one changes nothing)
#' kcs20fh[1:2,'gene_name'] <- kcs20fh[1:2,'gene_name']
#' 
#' # clean up
#' unlink(tf, recursive=TRUE)
#' 
"[.neuronlistfh" <- function(x, i, j, drop) {
  # treat like a data.frame
  if(nargs()>2) {
    return(NextMethod())
  }
  if(!is.character(i)) i=names(x)[i]
  l=sapply(i, function(n) x[[n]], simplify = F, USE.NAMES = T)
  new=as.neuronlist(l, df=attr(x, 'df')[i, , drop=FALSE])
  copy_nl_attributes(new, x, ignoremore=c("keyfilemap", "db"))
}

# Called if some objects in the filehash object are not available locally
# 
# @param missing A list of missing objects
# @param fh The neuronlistfh object that needs filling in
# @param quiet Whether to show download progress for each neuron
# @param progress 
fillMissing <- function(missing, fh, quiet=length(missing)>1, 
                        progress=getOption('nat.progress', default='auto')) {
  objDir <- attr(fh, 'db')@dir
  if (!file.exists(objDir)) dir.create(objDir)
  
  if(progress=='auto') progress=ifelse(interactive(), 'text', 'none')
  # convert to logical
  progress=progress!='none'
  
  if(progress){
    p <- progress_natprogress()
    p$init(length(missing))
    message('Downloading ',length(missing),' missing neurons from ',
            attr(fh, 'remote'))
  }
  
  for(i in seq_along(missing)){
    download.file(url=paste0(attr(fh, 'remote'), missing[i]),
                  destfile=file.path(objDir,missing[i]),
                  quiet=quiet, mode = 'wb')
    if(progress) p$step()
  }
}

#' Read a local, or remote, neuronlistfh object saved to a file.
#' 
#' @details When reading a remote \code{neuronlistfh} object, it is downloaded 
#'   and cached to \code{localdir}. If there is already a cached file at the 
#'   appropriate location and \code{update=TRUE} then the md5sums are checked 
#'   and the downloaded file will be copied on top of the original copy if they 
#'   are different; if \code{udpate=FALSE}, the default, then no action will be 
#'   taken. After downloading a remote \code{neuronlistfh} object, a check is 
#'   made for the existence of the \code{data} directory that will be used to 
#'   individual objects. If this does not exist it will be created.
#'   
#'   Note also that there is a \emph{strict convention} for the layout of the 
#'   files on disk. The neuronlistfh object will be saved in R's \code{RDS} 
#'   format and will be placed next to a folder called \code{data} which will 
#'   contain the data objects, also saved in RDS format. For example if 
#'   \code{myneurons.rds} is downloaded to 
#'   \code{localdir="\\path\\to\\localdir"} the resultant file layout will be as
#'   follows:
#'   
#'   \itemize{
#'   
#'   \item \code{\\path\\to\\localdir\\myneurons.rds}
#'   
#'   \item \code{\\path\\to\\localdir\\data\\2f88e16c4f21bfcb290b2a8288c05bd0}
#'   
#'   \item \code{\\path\\to\\localdir\\data\\5b58e040ee35f3bcc6023fb7836c842e}
#'   
#'   \item \code{\\path\\to\\localdir\\data\... etc}
#'   
#'   }
#'   
#'   Given this arrangement, the data directory should always be at a fixed 
#'   location with respect to the saved neuronlistfh object and this is enforced
#'   on download and the default behaviour on read and write. However it does 
#'   remain possible (if not recommended) to site the neuronlistfh and filehash 
#'   database directory in different relative locations; if the neuronlistfh 
#'   object specified by file does not have a filehash database with a valid 
#'   \code{dir} slot and there is no 'data' directory adjacent to the 
#'   neuronlistfh object, an error will result.
#' @param file The file path of the neuronlistfh object. Can be local, or remote
#'   (via http or ftp).
#' @param localdir If the file is to be fetched from a remote location, this is 
#'   the folder in which downloaded RDS file will be saved. The default value of
#'   \code{NULL} will save to a folder in the current R sessions temporary
#'   folder. See details.
#' @param update Whether to update local copy of neuronlistfh (default: FALSE, 
#'   see details)
#' @param ... Extra arguments to pass to \code{download.file}.
#' @export
#' @importFrom tools md5sum
#' @family neuronlistfh
read.neuronlistfh <- function(file, localdir=NULL, update=FALSE, ...) {
  if (substr(file, 1, 7) == "http://" || substr(file, 1, 6) == "ftp://") {
    if(is.null(localdir)) {
      localdir=file.path(tempdir(), 'nat')
      message("localdir not specified. Using a temporary folder for this R session!")
    }
    if(!file.exists(localdir)) dir.create(localdir, recursive=TRUE)

    cached.neuronlistfh<-file.path(localdir,basename(file))
    
    if(!file.exists(cached.neuronlistfh) || update){
      tmpFile <- tempfile()
      on.exit(unlink(tmpFile))
      download.file(url=file, destfile=tmpFile, mode='wb', ...)
      obj <- readRDS(tmpFile)
      
      # fix paths in our new object
      dbdir<- file.path(localdir,'data')
      attr(obj, 'db')@dir <- dbdir
      # make the local directory that will contain in
      if(!file.exists(dbdir)) dir.create(dbdir, recursive=TRUE)
      attr(obj, 'remote') <- paste0(dirname(file), '/data/')
      attr(obj, 'file') <- cached.neuronlistfh
      
      # save it to disk
      saveRDS(obj,file=tmpFile)
      # and copy / replace existing copy
      if(!file.exists(cached.neuronlistfh) || 
           isTRUE(all(md5sum(cached.neuronlistfh)!=md5sum(tmpFile)))){
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
  db <- attr(obj, 'db')
  if(inherits(db, 'filehashDB1')) {
    datafile=db@datafile
    if(!file.exists(datafile)) {
      # look right next door
      datafile2=file.path(dirname(file), basename(datafile))
      if(!file.exists(datafile2))
        stop("Unable to locate data file ", basename(datafile), "in dirs: ", 
             dirname(datafile), ' or: ', dirname(datafile2))
      attr(obj, 'db')@datafile <- datafile2
    }
    
  } else {
    dbdir=db@dir
    if(!isTRUE(file.info(dbdir)$isdir)){
      dbdir2 <- file.path(dirname(file),'data')
      if(!isTRUE(file.info(dbdir2)$isdir))
        stop("Unable to locate data directory at: ", dbdir, ' or: ', dbdir2)
      attr(obj, 'db')@dir <- dbdir2
    }
  }
  
  attr(obj, 'file') <- file
  obj
}


#' Write out a neuronlistfh object to an RDS file
#' 
#' @details This function writes the main neuronlistfh object to disk, but makes
#'   no attempt to touch/verify the associated object files.
#'   
#'   if \code{file} is not specified, then the function will first check if 
#'   \code{x} has a \code{'file'} attribute. If that does not exist, then 
#'   \code{attr(x,'db')@@dir}, the backing \code{filehash} database directory, 
#'   is inspected. The save path \code{file} will then be constructed by taking 
#'   the directory one up from the database directory and using the name of the 
#'   neuronlistfh object with the suffix '.rds'. e.g. write.neuronlistfh(kcs20) 
#'   with db directory '/my/path/dps/data' will be saved as 
#'   '/my/path/dps/kcs20.rds'
#'   
#'   Note that if x has a \code{'file'} attribute (set by 
#'   \code{read.neuronlistfh}) then this will be removed before the file is 
#'   saved (since the file attribute must be set on read to ensure that we know 
#'   exactly which file on disk was the source of the object in memory).
#' @param x The neuronlistfh object to write out
#' @param file Path where the file will be written (see details)
#' @param overwrite Whether to overwrite an existing file
#' @param \dots Additional parameters passed to \code{saveRDS}
#' @seealso \code{\link{saveRDS}}
#' @family neuronlistfh
#' @export
write.neuronlistfh<-function(x, file=attr(x,'file'), overwrite=FALSE, ...){
  if(is.null(file)) {
    db=attr(x, 'db')
    file <- if(inherits(db, "filehashRDS") || inherits(db, "filehashRDS2")) {
      dbdir=db@dir
      file.path(dirname(dbdir), paste0(as.character(substitute(x)),'.rds'))
    } else if(inherits(db, "filehashDB1")) {
      datafile=db@datafile
      file.path(paste0(datafile, '.rds'))
    } else stop("Unsupported neuronlistfh backend: ", class(db))
  }
  # check that we can write to this location
  dir_exists=file.exists(dirname(file))
  if(!dir_exists) stop("output directory does not exist")
  if(file.exists(file) && !overwrite) 
    stop("Set overwrite=TRUE to overwrite existing neuronlistfh")
  # set file attribute to NULL on way out
  if(!is.null(attr(x,'file'))) attr(x,'file')=NULL
  saveRDS(x, file=file, ...)
  invisible(file)
}

#' Synchronise a remote object
#' 
#' @param x Object to synchronise with a remote URL
#' @param remote The remote URL to update from
#' @param download.missing Whether to download missing objects (default TRUE)
#' @param delete.extra Whether to delete objects (default TRUE)
#' @param \dots Additional arguments passed to methods
#' @export
#' @family neuronlistfh
remotesync<-function(x, remote=attr(x,'remote'), download.missing=TRUE, 
                     delete.extra=FALSE, ...) UseMethod("remotesync")

#' @export
remotesync.default<-function(x, remote=attr(x,'remote'), download.missing=TRUE,
                             delete.extra=FALSE, ...){
  if(is.character(x)) x=read.neuronlistfh(x)
  
  if(!is.neuronlistfh(x))
    stop("Unable to update object of class", class(x))
  
  remotesync(x, remote=remote, download.missing=download.missing,
             delete.extra=delete.extra, ...)
}

#' @export
#' @param indices Character vector naming neurons to update (default
#'   \code{indices=NULL} implies all neurons).
#' @param update.object Whether to update the \code{neuronlistfh} object itself 
#'   on disk (default TRUE). Note that this assumes that the \code{neuronlistfh}
#'   object has not been renamed after it was downloaded.
#' @return The updated \code{neuronlistfh} object (invisibly)
#' @rdname remotesync
#' @examples
#' \dontrun{
#' kcs20=read.neuronlistfh('http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds')
#' # update object from the web
#' kcs20=remotesync(kcs20)
#' # download all neurons with significant innervation of the vertical lobe
#' mbvl_neurons=subset(kcs20, (MB_VL_R+MB_VL_L)>200, rval='names')
#' kcs20=remotesync(kcs20, indices=mbvl_neurons, download.missing=TRUE)
#' }
remotesync.neuronlistfh<-function(x, remote=attr(x,'remote'),
                                  download.missing=FALSE, delete.extra=FALSE,
                                  indices=NULL, update.object=TRUE, ...) {
  # first update the neuronlist object on disk
  if(update.object){
    # construct url to neuronlistfh object from remote data directory
    remoteurl_nlfh=paste0(dirname(remote),'/',basename(attr(x,'file')))
    x=read.neuronlistfh(remoteurl_nlfh, localdir=dirname(attr(x,'file')), update=TRUE)
  }
  
  if(download.missing || delete.extra) {
    db=attr(x, 'db')
    keyfilemap=attr(x, 'keyfilemap')
    if(!is.null(indices))
      keyfilemap=keyfilemap[intersect(names(keyfilemap), indices)]
    objects_present=dir(db@dir)

    if(download.missing){
      objects_missing=setdiff(keyfilemap, objects_present)
      if(length(objects_missing))
        fillMissing(objects_missing, x)
    }
    
    if(delete.extra){
      objects_extra=setdiff(objects_present, keyfilemap)
      if(length(objects_extra))
        unlink(file.path(db@dir, objects_extra))
    }
  }
  
  invisible(x)
}
