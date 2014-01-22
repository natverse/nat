# define neuronlistfh, that looks like a regular in memory neuronlist
# but is actually backed by an on disk filehash structure

#' neuronlistfh class to store multiple neurons cached on disk
#'  
#' neuronlistfh objects consist of a list of neuron objects along with an optional
#' attached dataframe containing information about the neurons. Relevant
#' functions include
#' plot3d.neuronlistfh
#' @name neuronlistfh
#' @family neuronlistfh
#' @seealso neuronlist
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
as.neuronlistfh<-function(x, df, ...)
  UseMethod("as.neuronlistfh")

#' @description \code{as.neuronlistfh.neuronlist} converts a regular neuronlist
#'   to one backed by a filehash object
#' @param dbName The name of the underlying filehash database on disk
#' @param filehash.type The filehash storage type
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
  db=dumpList(x, dbName=dbName, type=filehash.type)
  as.neuronlistfh.filehash(db, df, ...)
}

#' @description \code{as.neuronlistfh.filehash} wrap an existing filehash object
#'   (on disk) into a neuronlistfh
#' @method as.neuronlistfh filehash
#' @S3method as.neuronlistfh filehash
#' @rdname neuronlistfh
as.neuronlistfh.filehash<-function(x, df, ...){
  nlfh=as.neuronlist(vector(length=length(x)))
  names(nlfh)=names(x)
  class(nlfh)=c('neuronlistfh',class(nlfh))
  attr(nlfh,'db')=x
  if(!missing(df)) attr(nlfh,'df')=df
  nlfh
}

#' convert neuronlistfh to a regular (in memory) neuronlist
#' @method as.neuronlist neuronlistfh
#' @S3method as.neuronlist neuronlistfh
#' @inheritParams as.neuronlist
#' @param df An (optional) dataframe with information about each element of the
#'   list
as.neuronlist.neuronlistfh<-function(l, df, ...){
  if(!missing(df)) {
    # check compatibility
    if(nrow(df)!=length(l)) stop("df must have the same number of rows as",
                                 " there are elements in x")
    attr(l,'df')=df
  }
  # get the overloaded subscripting operator to do the work
  l[names(l)]
}

#' @S3method [[ neuronlistfh
"[[.neuronlistfh"<-function(x,i,...){
  if(!is.character(i)) i=names(x)[i]
  attr(x,'db')[[i,...]]
}

#' Apply a function over a neuronlistfh
#' 
#' @method lapply neuronlistfh
#' @export
#' @param X A neuronlistfh object
#' @param FUN a function to apply to each element of X
#' @param ... Arguments for fun passed on to lapply.filehash
lapply.neuronlistfh<-function(X, FUN, ...){
	db=attr(X,'db')
	lapply(db, FUN,...)
}

#' @S3method as.list neuronlistfh
as.list.neuronlistfh<-function(x, ...) {
  attr(x,'db')
}

#' Methods for neuronlistfh objects
#'
#' @export
#' @param X A neuronlistfh object
#' @param FUN a function to apply to each element of X
#' @param ... Additional arguments for FUN
#' @docType methods
#' @rdname neuronlistfh-methods
#' @importFrom methods setMethod
setMethod("lapply", signature(X = "neuronlistfh"),{ function(X, FUN, ...) lapply(attr(X,'db'), FUN, ...)})

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
