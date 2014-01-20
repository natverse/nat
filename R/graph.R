#' Make SegList (and other core fields) from full graph of all nodes and origin
#' 
#' @details Uses a depth first search on the tree to reorder using the given 
#'   origin.
#' @details When the graph contains multiple subgraphs, only one will be chosen 
#'   as the master tree and used to construct the SegList of the resultant 
#'   neuron. However all subgraphs will be listed in the SubTrees element of the
#'   neuron and nTrees will be set appropriately.
#' @details When the graph vertices have a label attribute derived from PointNo,
#'   the origin is assumed to be specified with respect to the vertex labels
#'   rather than the raw vertex ids.
#' @param g An igraph
#' @param origin Root vertex, matched against labels (aka PointNo) when 
#'   available (see details)
#' @return A list with elements: 
#'   (NumPoints,StartPoint,BranchPoints,EndPoints,nTrees,NumSegs,SegList,
#'   [SubTrees])
#'   NB SubTrees will only be present when nTrees>1.
#' @export
#' @rdname CoreNeuron
#' @seealso \code{\link{graph.dfs},\link{RerootNeuron},\link{graph2seglist}}
CoreNeuronFromGraph<-function(g, origin=NULL, Verbose=TRUE){
  # translate origin into raw vertex id if necessary 
  if(!is.null(origin)){
    vertex_labels=igraph::V(g)$label
    if(!is.null(vertex_labels)){
      origin=match(origin,vertex_labels)
      if(is.na(origin)) stop("Invalid origin")
    }
  }
  # save original vertex ids
  igraph::V(g)$vid=seq.int(igraph::vcount(g))
  # check if we have multiple subgraphs
  if(no.clusters(g)>1){
    if(is.null(origin)){
      # no origin specified, will pick the biggest subtree
      # decompose into list of subgraphs
      gg=igraph::decompose.graph(g)
      # reorder by descending number of vertices
      gg=gg[order(sapply(gg,igraph::vcount), decreasing=TRUE)]
      subtrees=lapply(gg, graph2seglist, Verbose=Verbose)
      sl=subtrees[[1]]
      masterg=gg[[1]]
    } else {
      # origin specified, subtree containing origin will be the master
      cg=igraph::clusters(g)
      master_tree_num=cg$membership[origin]
      # make a master graph with the vertices from subgraph including origin
      masterg=igraph::induced.subgraph(g, which(cg$membership==master_tree_num))
      # ... and then corresponding seglist
      sl=graph2seglist(masterg, origin=origin)
      # now deal with remaining vertices
      remainderg=igraph::induced.subgraph(g, which(cg$membership!=master_tree_num))
      gg=igraph::decompose.graph(remainderg)
      # reorder by descending number of vertices
      gg=gg[order(sapply(gg,igraph::vcount), decreasing=TRUE)]
      subtrees=c(list(sl),lapply(gg, graph2seglist, Verbose=Verbose))
    }
    nTrees=length(subtrees)
  } else {
    # this is a well-behaved graph that is immediately ready to be master graph
    # of neuron
    sl=graph2seglist(masterg<-g, origin=origin, Verbose=Verbose)
    nTrees=1
  }
  if(length(sl)==0 || length(sl[[1]])<2)
    stop("Invalid neuron! Must contain at least one segment with 2 points")
  # Finalise StartPoint - should always be head point of first segment
  StartPoint=sl[[1]][1]
  
  ncount=igraph::degree(masterg)
  n=list(NumPoints=length(ncount),
         StartPoint=StartPoint,
         BranchPoints=branchpoints(masterg, original.ids='vid'),
         EndPoints=endpoints(masterg, original.ids='vid'),
         nTrees=nTrees,
         NumSegs=length(sl),
         SegList=sl)
  if(nTrees>1) n=c(n,list(SubTrees=subtrees))
  n
}

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
