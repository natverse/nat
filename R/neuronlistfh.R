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
NULL

library(filehash)

#' test if an object is a neuronlistfh
is.neuronlistfh<-function(nl) {
  inherits(nl,"neuronlistfh")
}

#' generic function to convert an object to neuronlistfh
#' @param x Object to convert
as.neuronlistfh<-function(l,df,...)
  UseMethod("as.neuronlistfh")

#' convert a regular neuronlist to one backed by a filehash object
as.neuronlistfh.neuronlist<-function(l,df,...,dbName='nldb',type='RDS'){
  if(is.null(names(l))){
    warning("giving default names to elements of nl")
    names(l)=seq(l)
  }
  db=dumpList(l,dbName=dbName,type=type)
  as.neuronlistfh.filehash(db,df,...)
}

#' wrap a filehash object into a neuronlistfh
as.neuronlistfh.filehash<-function(x,df,...){
  nlfh=as.neuronlist(vector(length=length(x)))
  names(nlfh)=names(x)
  class(nlfh)=c('neuronlistfh',class(nlfh))
  attr(nlfh,'db')=x
  if(!missing(df)) attr(nlfh,'df')=df
  nlfh
}

#' convert neuronlistfh to a regular (in memory) neuronlist
as.neuronlist.neuronlistfh<-function(x,df,...){
  if(!missing(df)) {
    # check compatibility
    if(nrow(df)!=length(x)) stop("df must have the same number of rows as",
                                 " there are elements in x")
    attr(x,'df')=df
  }
  # get the overloaded subscripting operator to do the work
  x[names(x)]
}

#' extract an element from a neuronlistfh
"[[.neuronlistfh"<-function(x,i,...){
  if(!is.character(i)) i=names(x)[i]
  attr(x,'db')[[i,...]]
}

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
"[.neuronlistfh" <- function(x,i,...) {
  if(!is.character(i)) i=names(x)[i]
  as.neuronlist(attr(x,'db')[i,...],df=attr(x,'df'))
}

#' plot neurons stored in a neuronlistfh
plot3d.neuronlistfh<-function(x,...){
  NextMethod('plot3d')
}
