# define neuronlistfh, that looks like a regular in memory neuronlist
# but is actually backed by an on disk filehash structure

#' neuronlistfh class to store multiple neurons cached on disk
#' 
#' \code{neuronlistfh} objects consist of a list of neuron objects along with an
#' optional attached dataframe containing information about the neurons. 
#' \code{neuronlistfh} objects also inherit from \code{neuronlist} and therefore
#' any appropriate methods e.g. \code{plot3d.neuronlist} can also be used on 
#' \code{neuronlistfh} objects.
#' @details Note that objects are stored in a filehash, which by definition does
#'   not have any ordering of its elements. However neuronlist onbjects (like 
#'   lists) do have an ordering. Therefore the names of a neuronlistfh object
#'   are not necessarily the same as the result of names on the underlying
#'   filehash object.
#' @name neuronlistfh
#' @family neuronlistfh
#' @family neuronlist
#' @import filehash
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
#' # create on disk filehash with one file per neuron
#' kcs20fh=as.neuronlistfh(kcs20,dbName='/path/to/my/kcdb',filehash.type='RDS')
#' plot3d(subset(kcs20fh,type=='gamma'))
#' 
#' # in a new session
#' kcs20fh=neuronlist(dbName='/path/to/my/kcdb')
#' }
as.neuronlistfh<-function(x, df, ...)
  UseMethod("as.neuronlistfh")

#' @param dbName The name of the underlying filehash database on disk
#' @param filehash.type The filehash storage type
#' @description \code{as.neuronlistfh.neuronlist} converts a regular neuronlist 
#'   to one backed by a filehash object with an on disk representation
#' @method as.neuronlistfh neuronlist
#' @S3method as.neuronlistfh neuronlist
#' @rdname neuronlistfh
as.neuronlistfh.neuronlist<-function(x, df=attr(x,'df'), ..., dbName='nldb', 
                                     filehash.type='RDS'){
  if(is.null(names(x))){
    warning("giving default names to elements of x")
    names(x)=seq(x)
  }
  if(missing(df)) df=attr(x,'df')
  db=filehash::dumpList(x, dbName=dbName, type=filehash.type)
  as.neuronlistfh(db, df, ...)
}

#' @description \code{as.neuronlistfh.filehash} wrap an existing filehash object
#'   (on disk) into a neuronlistfh
#' @method as.neuronlistfh filehash
#' @S3method as.neuronlistfh filehash
#' @rdname neuronlistfh
#' @details In \code{as.neuronlistfh.filehash} the dataframe determines the ordering of the objects in
as.neuronlistfh.filehash<-function(x, df, ...){
  nlfh=as.neuronlist(vector(length=length(x)))
  attr(nlfh,'db')=x
  class(nlfh)=c('neuronlistfh',class(nlfh))
  
  if(missing(df)) {
    names(nlfh)=names(x)
  } else {
    if(nrow(df)!=length(x))
      stop("data.frame must have same number of rows as elements in x")
    names(nlfh)=rownames(df)
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
  attr(x,'db')[[i,...]]
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
  as.neuronlist(attr(x,'db')[i,...],df=attr(x,'df')[i,])
}
