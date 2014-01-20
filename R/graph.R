#' Convert a fully connected graph into a seglist
#' 
#' @details If the graph vertices have \code{vid} attributes, typically defining
#'   the original vertex ids of a graph that was then decomposed into subgraphs,
#'   then the origin is assumed to refer to one of these vids not a raw vertex 
#'   id of the current graph. The returned seglist will also contain these 
#'   original vertex ids.
#' @details The head of the first segment in the seglist will be the origin.
#' @param g A fully connected \code{igraph}
#' @param origin The origin of the tree (see details)
#' @return a \code{list} with one entry for each unbranched segment.
graph2seglist<-function(g, origin=NULL, Verbose=FALSE){
  # Handle degenerate cases
  if(!is.connected(g)) stop("Graph is not fully connected!")
  if(igraph::vcount(g)==0) {
    if(Verbose) warning("Empty graph! Seglist not defined")
    return(NULL)
  }
  if(igraph::is.directed(g) && !igraph::is.dag(g)){
    stop("Graph has cycles!")
  }
  
  # Handle Vertex ids
  # If this is a subgraph we need to keep track of original vertex ids. The 
  # simplest thing to do is to make a fake set of original vertex ids if they
  # are not present and _always_ translate (since this is fast) rather than
  # having separate program logic
  vids=igraph::V(g)$vid
  if(is.null(vids)){
    vids=seq.int(igraph::vcount(g))
  }
  
  # Floating point
  if(igraph::vcount(g)==1) {
    return(list(vids))
  }
  
  # Handle Origin
  if(is.null(origin)){
    # no explicit origin specified, use raw vertex id of graph root
    if(is.directed(g))
      origin=rootpoints(g, original.ids=FALSE)
  } else {
    # we've been given an origin but it may not be a raw vertex id for this
    # graph so translate it
    origin=match(origin,vids)
  }
  if(length(origin)==0){
    warning("No valid origin found! Using first endpoint as origin")
    # nb we just want the raw vertex id for this graph, so original.ids = FALSE
    origin=endpoints(g, original.ids=FALSE)[1]
  } else if(length(origin)>1){
    warning("Multiple origins found! Using first origin.")
    origin=origin[1]
  }
  
  # Now do a depth first search to ensure that ordering is correct
  dfs=igraph::graph.dfs(g, root=origin, father=TRUE, neimode='all')
  ncount=igraph::degree(g)
  # put the first vertex into the first segment
  # note that here and elsewhere the points stored in curseg will be the
  # _original_ vertex ids specified by "vid" attribute of input graph
  curseg=vids[dfs$order[1]]
  if(length(ncount)==1) stop("Unexpected singleton point found!")
  sl=list()
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
