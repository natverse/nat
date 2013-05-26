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
  db=attr(x,'db')
  if(missing(df)) df=attr(x,'df')
  nl=as.neuronlist(dbMultiFetch(db,names(db)),df,...)
  nl
}

#' extract an element from a neuronlistfh
"[[.neuronlistfh"<-function(x,i,...){
  if(!is.character(i)) i=names(x)[i]
  attr(x,'db')[[i,...]]
}

#' extract a sublist from a neuronlistfh
#'
#' Hmm should I be keeping the same backing database - fine for read only
#' but not such a good idea if I'm going to write!
"[.neuronlistfh" <- function(x,i,...) {
  nl=structure(NextMethod("["), class = class(x))
  db=attr(x,'db')
  if(!is.character(i)) i=names(db)[i]
  attr(nl,'db')=attr(x,'db')[i,...]
  nl
}

#' plot neurons stored in a neuronlistfh
plot3d.neuronlistfh<-function(x,...){
  NextMethod('plot3d')
}
