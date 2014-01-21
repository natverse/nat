#' Make/convert neuron connectivity information into a seglist object
#' 
#' @description \code{seglist} makes a seglist object from a list of integer 
#'   vectors of raw vertex ids. As a convenience if a vector of numeric ids are
#'   passed these are assumed to specify a neuron with 1 segment.
#' @details see \code{\link{neuron}} for further information about seglists.
#' @param ... for \code{seglist} integer vectors to convert to a seglist
#' @return A \code{list} with additional class \code{seglist}.
#' @export
#' @examples
#' sl=seglist(c(1:2),c(2:6))
seglist<-function(...) {
  sl=list(...)
  if(!inherits(sl,'seglist')) class(sl)=c("seglist",class(sl))
  sl
}

#' @rdname seglist
#' @param x object passed to be converted to seglist
#' @export
#' @seealso \code{\link{neuron}}
as.seglist<-function(x, ...) UseMethod('as.seglist')

#' @S3method as.seglist seglist
as.seglist.seglist<-function(x, ...) x

#' @S3method as.seglist list
as.seglist.list<-function(x, ...) {
  if(isTRUE(class(x)=='list')) class(x)=c("seglist",class(x))
  x
}

#' @S3method as.seglist default
as.seglist.default<-function(x, ...) stop("Not yet implemented!")

#' @description \code{as.seglist.igraph} will convert a fully connected acyclic 
#'   ngraph or igraph object into a seglist consisting of exactly one subtree.
#' @details If the graph vertices have \code{vid} attributes, typically defining
#'   the original vertex ids of a graph that was then decomposed into subgraphs,
#'   then the origin is assumed to refer to one of these vids not a raw vertex 
#'   id of the current graph. The returned seglist will also contain these 
#'   original vertex ids.
#' @details The head of the first segment in the seglist will be the origin.
#' @param origin The origin of the tree (see details)
#' @param Verbose Whether to print progress updates to console (default FALSE)
#' @return a \code{list} with one entry for each unbranched segment.
#' @seealso \code{\link{ngraph},\link{igraph}}
#' @S3method as.seglist igraph
#' @method as.seglist igraph
#' @rdname seglist
as.seglist.igraph<-function(x, origin=NULL, Verbose=FALSE, ...){
  # Handle degenerate cases
  if(!is.connected(x)) stop("Graph is not fully connected!")
  if(igraph::vcount(x)==0) {
    if(Verbose) warning("Empty graph! Seglist not defined")
    return(NULL)
  }
  if(igraph::is.directed(x) && !igraph::is.dag(x)){
    stop("Graph has cycles!")
  }
  
  # Handle Vertex ids
  # If this is a subgraph we need to keep track of original vertex ids. The 
  # simplest thing to do is to make a fake set of original vertex ids if they
  # are not present and _always_ translate (since this is fast) rather than
  # having separate program logic
  vids=igraph::V(x)$vid
  if(is.null(vids)){
    vids=seq.int(igraph::vcount(x))
  }
  
  # Floating point
  if(igraph::vcount(x)==1) {
    return(seglist(vids))
  }
  
  # Handle Origin
  if(is.null(origin)){
    # no explicit origin specified, use raw vertex id of graph root
    if(is.directed(x))
      origin=rootpoints(x, original.ids=FALSE)
  } else {
    # we've been given an origin but it may not be a raw vertex id for this
    # graph so translate it
    origin=match(origin,vids)
  }
  if(length(origin)==0){
    warning("No valid origin found! Using first endpoint as origin")
    # nb we just want the raw vertex id for this graph, so original.ids = FALSE
    origin=endpoints(x, original.ids=FALSE)[1]
  } else if(length(origin)>1){
    warning("Multiple origins found! Using first origin.")
    origin=origin[1]
  }
  
  # Now do a depth first search to ensure that ordering is correct
  dfs=igraph::graph.dfs(x, root=origin, father=TRUE, neimode='all')
  ncount=igraph::degree(x)
  # put the first vertex into the first segment
  # note that here and elsewhere the points stored in curseg will be the
  # _original_ vertex ids specified by "vid" attribute of input graph
  curseg=vids[dfs$order[1]]
  if(length(ncount)==1) stop("Unexpected singleton point found!")
  sl=seglist()
  # we have more than 1 point in graph and some work to do!
  for(i in seq.int(from=2,to=length(dfs$order))){
    curpoint=dfs$order[i]
    if(length(curseg)==0){
      # segment start, so initialise with parent
      curseg=vids[dfs$father[curpoint]]
    }
    # always add current point
    curseg=c(curseg,vids[curpoint])
    # now check if we need to close the segment
    if(ncount[curpoint]!=2){
      # branch or end point
      sl[[length(sl)+1]]=curseg
      curseg=integer(0)
    }
  }
  sl
}

#' Recalculate Neurons's SWCData using SegList and point information
#'
#' Uses the SegList field (indices into point array) to recalculate 
#' point numbers and parent points for SWC data field (d).
#' If Label column is missing (or ReplaceLabel=TRUE) then it is set to the
#' value of DefaultLabel (2=Axon by default).
#' Note that the order of point indices in SegList must match those in SWC.
#' @param x Neuron containing both the SegList and d fields or a plain seglist
#' @param d SWC data block (only expected if x is a SegList)
#' @param RecalculateParents Whether to recalculate parent points (default T)
#' @param DefaultLabel Integer label to use for SWC data chunk
#' @param ReplaceLabel Whether to replace Label column if it already exists
#' @return A neuron if x was a neuron otherwise dataframe of swc data
#' @export
seglist2swc<-function(x, d, RecalculateParents=TRUE, DefaultLabel=2L,
                      ReplaceLabel=FALSE){
  if(missing(d)){
    if(!is.neuron(x)) stop("Must supply x=neuron or x=SegList and d=SWC data")
    d=x$d
    if(isTRUE(x$nTrees>1))
      sl=unlist(x$SubTrees, recursive=FALSE)
    else sl=x$SegList
  } else {
    sl=x
    # is this a plain SegList or a list of seglists
    if(!is.null(sl[[1]][[1]]))
      sl=unlist(sl, recursive=FALSE)
  }
  if(is.null(d$PointNo)){
    if(nrow(d)==0) stop("Must supply either supply some coords or PointNos")
    d$PointNo=seq(nrow(d))
  }
  if(is.null(d$Parent) || RecalculateParents){
    d$Parent=-1L
    for(s in sl){
      # first handle length 1 segments i.e. floating points
      if(length(s)==1) {
        d$Parent[s]=-1
      } else if (length(s)>1){
        # NB points in s are raw vertex ids corresponding to rows in the data
        # block, but SWC Parent is expressed in PointNos
        d$Parent[s[-1]]=d$PointNo[s[-length(s)]]
      }
    }
  }
  if(is.null(d$Label) || ReplaceLabel)
    d$Label=DefaultLabel
  if(is.neuron(x)){
    x$d=d
    x
  } else d
}
