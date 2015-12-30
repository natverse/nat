#' ngraph: a graph to encode a neuron's connectivity
#' 
#' @description the \code{ngraph} class contains a (completely general) graph 
#'   representation of a neuron's connectivity in an \code{igraph} object. It 
#'   may additionally contain vertex label or position data. See details.
#'   
#'   \code{ngraph()} creates an ngraph from edge and vertex information.
#' @section Connectivity: We make the following assumptions about neurons coming
#'   in
#'   
#'   \itemize{
#'   
#'   \item They have an integer vertex label that need not start from 1 and that
#'   may have gaps
#'   
#'   \item The edge list which defines connectivity specifies edges using pairs 
#'   of vertex labels, _not_ raw vertex ids.
#'   
#'   }
#'   
#'   We make no attempt to determine the root points at this stage.
#'   
#'   The raw vertex ids in the graph will be in the order of vertexlabels and 
#'   can therefore be used to index a block of vertex coordinates. The 
#'   vertexlabels will be stored using the vertex attribute \code{label}
#'   
#'   When the graph is directed (default) the edges will be from the root to the
#'   other tips of the neuron.
#' @section Morphology: The morphology of the neuron is encoded by the 
#'   combination of connectivity information (i.e. the graph) and spatial data 
#'   encoded as the 3D position and diameter of each vertex. Position 
#'   information is stored as vertex attributes X, Y, and Z.
#' @param el A two columm matrix (start, end) defining edges. \code{start} means
#'   closer to the root (soma) of the neuron.
#' @param vertexlabels Integer labels for graph - the edge list is specified 
#'   using these labels.
#' @param xyz 3D coordinates of vertices (optional, Nx3 matrix, or Nx4 matrix 
#'   when 4th column is assumed to be diameter)
#' @param diam Diameter of neuron at each vertex (optional)
#' @param weights Logical value indicating whether edge weights defined by the 
#'   3D distance between points should be added to graph (default \code{FALSE}) 
#'   \emph{or} a numeric vector of weights.
#' @param directed Whether the resultant graph should be directed (default TRUE)
#' @param vertex.attributes,graph.attributes List of named attributes to be 
#'   added to the graph. The elements of \code{vertex.attributes} must be 
#'   vectors whose length is compatible with the number of elements in the 
#'   graph. See \code{\link[igraph]{set.vertex.attribute}} for details.
#' @return an \code{igraph} object with additional class \code{ngraph}, having a
#'   vertex for each entry in vertexlabels, each vertex having a \code{label} 
#'   attribute. All vertices are included whether connected or not.
#' @family neuron
#' @seealso \code{\link{igraph}}, \code{\link[igraph]{set.vertex.attribute}},
#'   \code{\link{subset.neuron}} for example of graph-based manipulation of a
#'   neuron.
#' @export
#' @importFrom igraph V<-
#' @examples
#' g=as.ngraph(Cell07PNs[[1]])
#' library(igraph)
#' # check that vertex attributes of graph match X position
#' all.equal(V(g)$X, Cell07PNs[[1]]$d$X)
ngraph<-function(el, vertexlabels, xyz=NULL, diam=NULL, directed=TRUE,
                 weights=FALSE, vertex.attributes=NULL, graph.attributes=NULL){
  if(any(duplicated(vertexlabels))) stop("Vertex labels must be unique!")
  # now translate edges into raw vertex_ids
  rawel=match(t(el), vertexlabels)
  if(isTRUE(weights) && !is.null(xyz)){
    # rawel is no longer a matrix
    rawel.mat=matrix(rawel, nrow=2)
    starts=rawel.mat[1,]
    stops=rawel.mat[2,]
    # nb drop = FALSE to ensure that we always have a matrix
    vecs=xyz[stops, , drop=FALSE] - xyz[starts, , drop=FALSE]
    weights=sqrt(rowSums(vecs*vecs))
  }
  g=igraph::graph(rawel, n=length(vertexlabels), directed=directed)
  igraph::V(g)$label=vertexlabels
  if(is.numeric(weights))
    igraph::E(g)$weight=weights
  if(!is.null(xyz)) {
    if(ncol(xyz)==4 && is.null(diam)){
      diam=xyz[,4]
      xyz=xyz[, 1:3, drop=FALSE]
    }
    xyzmatrix(g)<-xyz
  }
  if(!is.null(diam)) V(g)$diam=diam
  for(n in names(vertex.attributes)){
    g=igraph::set.vertex.attribute(g,name=n,value=vertex.attributes[[n]])
  }
  for(n in names(graph.attributes)){
    g=igraph::set.graph.attribute(g,name=n,value=graph.attributes[[n]])
  }
  class(g)=c("ngraph",class(g))
  g
}

#' @description \code{as.ngraph} converts an object to an ngraph
#' 
#' @param x Object to convert (see method descriptions)
#' @param ... Arguments passed to methods
#' @export
#' @rdname ngraph
as.ngraph<-function(x, ...) UseMethod('as.ngraph')

#' @description \code{as.ngraph.dataframe} construct ngraph from a data.frame 
#'   containing SWC format data
#' @method as.ngraph data.frame
#' @export
#' @rdname ngraph
as.ngraph.data.frame<-function(x, directed=TRUE, ...){
  el=x[x$Parent!=-1,c("Parent","PointNo")]
  ngraph(data.matrix(el), x$PointNo, directed=directed, xyz=xyzmatrix(x), diam=x$W, ...)
}

#' @description \code{as.ngraph.neuron} construct ngraph from a neuron
#' @rdname ngraph
#' @export
#' @method as.ngraph neuron
#' @param method Whether to use the swc data (x$d) or the seglist to define 
#'   neuronal connectivity to generate graph.
#' @details Note that the \code{as.ngraph.neuron} method \emph{always} keeps the
#'   original vertex labels (a.k.a. PointNo) as read in from the original file.
as.ngraph.neuron<-function(x, directed=TRUE, method=c('swc','seglist'), ...){
  method=match.arg(method, several.ok=TRUE)
  if('swc'%in%method && !is.null(x$d$Parent) && !is.null(x$d$PointNo)){
    as.ngraph(x$d, directed=directed, ...)
  } else {
    as.ngraph(seglist2swc(x)$d, directed=directed, ...)
  }
}

#' @importFrom igraph as.undirected as.directed
#' @export
as.ngraph.igraph<-function(x, directed=TRUE, root, mode=c('out','in'), ...){
  if(inherits(x,'ngraph'))
    if(igraph::is.directed(x)==directed) return(x)
  
  if(igraph::is.directed(x) && !directed) x=as.undirected(x, ...)
  else if(!igraph::is.directed(x) && directed) x=as.directed.usingroot(x, root, mode=mode, ...)
  
  if(!inherits(x,'ngraph')){
    class(x)=c("ngraph",class(x))
  }
  x
}

as.directed.usingroot<-function(g, root, mode=c('out','in')){
  mode=match.arg(mode)
  # make a directed graph _keeping any attributes_
  if(!igraph::is.directed(g))
    dg=igraph::as.directed(g, mode='arbitrary')
  else dg=g
  dfs=igraph::graph.dfs(dg, root, unreachable=FALSE, dist=TRUE, neimode='all')
  el=igraph::get.edgelist(dg)
  
  connected_vertices=which(is.finite(dfs$order))
  edges_to_check=which(el[,1]%in%connected_vertices)
  
  # for each edge, check if it must be flipped
  parent.dists=dfs$dist[el[edges_to_check,1]]
  child.dists=dfs$dist[el[edges_to_check,2]]
  #
  parent_closer=parent.dists<child.dists
  same_dist=parent.dists==child.dists
  parent_further=parent.dists>child.dists
  #
  if(any(same_dist)) warning(sum(same_dist)," edges connect vertices that are the same distance from the root => cycles.")
  edges_to_flip <- edges_to_check[if(mode=='out') parent_further else parent_closer]
  
  dg=igraph::delete.edges(dg,edges_to_flip)
  dg=igraph::add.edges(dg,t(el[edges_to_flip,2:1]))
  dg
}


#' Compute the longest path (aka spine or backbone) of a neuron
#' 
#' @param n the neuron to consider.
#' @param UseStartPoint Whether to use the StartPoint of the neuron (often the 
#'   soma) as the starting point of the returned spine.
#' @param SpatialWeights logical indicating whether spatial distances (default) 
#'   should be used to weight segments instead of weighting each edge equally.
#' @param LengthOnly logical indicating whether only the length of the longest 
#'   path should be returned (when \code{TRUE}) or whether a neuron pruned down 
#'   to the the sequence of vertices along the path should be returned 
#'   (\code{FALSE}, the default).
#' @return Either a neuron object corresponding to the longest path \emph{or} 
#'   the length of the longest path when \code{LengthOnly=TRUE}).
#' @seealso \code{\link[igraph]{diameter}}, 
#'   \code{\link[igraph]{shortest.paths}}, \code{\link{prune_strahler}} for 
#'   removing lower order branches from a neuron, \code{\link{prune}} for 
#'   removing parts of a neuron by spatial criteria.
#' @export
#' @examples
#' plot3d(Cell07PNs[[1]])
#' plot3d(spine(Cell07PNs[[1]]), lwd=4, col='black')
#' # just extract length
#' spine(Cell07PNs[[1]], LengthOnly=TRUE)
#' # same result since StartPoint is included in longest path
#' spine(Cell07PNs[[1]], LengthOnly=TRUE, UseStartPoint=TRUE)
#' @importFrom igraph shortest.paths get.shortest.paths diameter get.diameter 
#'   delete.vertices
spine <- function(n, UseStartPoint=FALSE, SpatialWeights=TRUE, LengthOnly=FALSE) {
  ng <- as.ngraph(n, weights=SpatialWeights)
  if(UseStartPoint) {
    # Find longest shortest path from given start point to all end points
    lps=shortest.paths(graph = ng, n$StartPoint, to = n$EndPoints, 
                       mode = 'all')
    if(LengthOnly) return(max(lps))
    to=n$EndPoints[which.max(lps)]
    longestpath=get.shortest.paths(ng, from = n$StartPoint, to = to, mode = 'all')$vpath[[1]]
  } else {
    if(LengthOnly) {
      return(diameter(ng, directed=FALSE))
    } else {
      longestpath=get.diameter(ng, directed=FALSE)
    }
  }
  spineGraph <- delete.vertices(ng, setdiff(V(ng), longestpath))
  as.neuron(as.ngraph(spineGraph), vertexData=n$d[match(V(spineGraph)$label,n$d$PointNo), ])
}

#' Return a simplified segment graph for a neuron
#' 
#' @details The resultant graph will contain all branch and endpoints of the 
#'   original neuron. This will be constructed from the SegList field, or where 
#'   present, the SubTrees field (containing multiple SegLists for each isolated
#'   graph in the neuron). Each edge in the output graph will match one segment 
#'   in the original SegList.
#' @param x neuron
#' @param weights Whether to include the original segment lengths as edge
#'   weights in the graph.
#' @param segids Whether to include the integer segment ids as an edge attribute
#'   in the graph
#' @param exclude.isolated Whether to eliminate isolated nodes
#' @param include.xyz Whether to include 3D location as vertex attribute
#' @param reverse.edges Whether to reverse the direction of each edge in the 
#'   output graph to point towards (rather than away from) the root (default 
#'   \code{FALSE})
#' @return \code{igraph} object containing only nodes of neuron keeping original
#'   labels (\code{x$d$PointNo} => \code{V(g)$label}) and vertex indices 
#'   (\code{1:nrow(x$d)} => \code{V(g)$vid)}.
#' @importFrom igraph graph.empty add.edges E
#' @export
#' @examples 
#' sg=segmentgraph(Cell07PNs[[1]])
#' str(sg)
#' library(igraph)
#' plot(sg, edge.arrow.size=.4, vertex.size=10)
segmentgraph<-function(x, weights=TRUE, segids=FALSE, exclude.isolated=FALSE, 
                       include.xyz=FALSE, reverse.edges=FALSE){
  g=graph.empty()
  pointnos=x$d$PointNo
  sts=as.seglist(x, all=TRUE, flatten = TRUE)
  topntail<-function(x) if(length(x)==1) x else x[c(1,length(x))]
  # just get head and tail of each segment
  simple_sts=lapply(sts,topntail)
  all_nodes=sort(unique(unlist(simple_sts)))
  # make empty graph with approriate nodes
  g=graph.empty(n=length(all_nodes))
  # store external pointnos
  igraph::V(g)$label=pointnos[all_nodes]
  # store original vertex ids
  igraph::V(g)$vid=all_nodes
  
  # handle the edges - first make list full edgelist
  el=EdgeListFromSegList(simple_sts)
  if(reverse.edges)
    el=el[,2:1]
  # convert from original vertex ids to vids of reduced graph
  elred=match(t(el),all_nodes)
  
  if(identical(segids, FALSE)) {
    segids=NULL
  } else if(isTRUE(segids)){
    segids=seq_along(sts)
  }
  if(weights){
    weights=seglengths(x, all=TRUE)
    g=add.edges(g, elred, weight=weights, segid=segids)
  } else {
    g=add.edges(g, elred, segid=segids)
  }
  
  if(include.xyz){
    igraph::V(g)$x=x$d$X[all_nodes]
    igraph::V(g)$y=x$d$Y[all_nodes]
    igraph::V(g)$z=x$d$Z[all_nodes]
  }
  if(exclude.isolated){
    # remove any points with no neighbours
    isolated_vertices=igraph::V(g)[igraph::degree(g)==0]
    g=igraph::delete.vertices(graph=g,isolated_vertices)
  }
  g
}

#' Find the Strahler order of each point in a neuron
#' 
#' @description The Strahler order will be 1 for each tip segment and then 1 + 
#'   the maximum of the Strahler order of each parent segment for internal 
#'   segments. Branch points will have the Strahler order of the closest segment
#'   to the root of which they are part.
#'   
#' @param x A neuron
#' @details It is vital that the root of the neuron is valid since this 
#'   determines the flow direction for calculation of the Strahler order. At 
#'   present the function is not defined for neurons with multiple subtrees.
#'   
#'   Internally, this function uses \code{\link{segmentgraph}} to find a reduced
#'   segmentgraph for the neuron.
#' @references \url{https://en.wikipedia.org/wiki/Strahler_number}
#' @export
#' @seealso \code{\link{prune_strahler}}, a \code{\link{segmentgraph}} (a form
#'   of \code{\link{ngraph}}) representation is used to calculate the Strahler 
#'   order.
#' @importFrom igraph bfs neighborhood V
#' @return A list containing \itemize{
#'   
#'   \item points Vector of integer Strahler orders for each point in the neuron
#'   
#'   \item segments Vector of integer Strahler orders for each segment in the 
#'   neuron }
strahler_order<-function(x){
  s=segmentgraph(x, weights = F)
  
  roots=rootpoints(s, original.ids=FALSE)
  if(length(roots)>1)
    stop("strahler_order not yet defined for multiple subtrees")
  
  b=bfs(s, root=roots, neimode = 'out', unreachable=F, father=T)
  
  # find neighbours for each node
  n=neighborhood(s, 1, mode='out')
  # empty vector to hold order for each node
  so_red_nodes=integer(vcount(s))
  
  # visit nodes in reverse order of bfs traversal
  # this ensures that we always visit children before parents
  for(i in rev(b$order)) {
    children=setdiff(n[[i]], i)
    if(length(children)==0L) {
      # terminal node
      so_red_nodes[i]=1L
      next
    }
    # we have some children
    child_orders=so_red_nodes[children]
    # if all have the same order, increment, otherwise choose the max
    if(length(children)==1L) {
      # this should be the root node
      so_red_nodes[i]=max(child_orders)
    } else if(max(child_orders)==min(child_orders)){
      so_red_nodes[i]=max(child_orders)+1L
    } else {
      so_red_nodes[i]=max(child_orders)
    }
  }
  
  # iterate over segments
  # finding head and tail node
  # all nodes in segment have min strahler order of head & tail
  so_orig_nodes=integer(length(nrow(x$d)))
  sts=as.seglist(x, all=TRUE, flatten = TRUE)
  so_segs=integer(length(sts))
  svids=V(s)$vid
  topntail<-function(x) if(length(x)==1) x else x[c(1,length(x))]
  for(i in seq_along(sts)){
    segends=topntail(sts[[i]])
    so_segends=so_red_nodes[match(segends, svids)]
    so_orig_nodes[segends]=so_segends
    so_this_seg=min(so_segends)
    so_segs[i]=so_this_seg
    
    internal=setdiff(sts[[i]], segends)
    if(length(internal)) {
      so_orig_nodes[internal]=so_this_seg
    }
  }
  list(points=so_orig_nodes, segments=so_segs)
}


#' Prune a neuron by removing segments with a given Strahler order
#' 
#' @param x A \code{neuron}
#' @param orderstoprune Integer indices of which Strahler orders to prune - 
#'   defaults to the lowest two orders (\code{1:2})
#' @param ... Additional arguments passed to \code{\link{as.neuron.data.frame}}
#'   
#' @return The pruned \code{neuron}
#' @export
#' @seealso \code{\link{strahler_order}}, \code{\link{spine}}, for finding the 
#'   longest path in a neuron, \code{\link{prune}} for subsetting 
#'   \code{dotprops} style neurons by spatial proximity, 
#'   \code{\link{as.neuron.data.frame}}, which is used to generate the new
#'   neuron.
#'   
#' @examples
#' x=Cell07PNs[[1]]
#' pruned12=prune_strahler(x)
#' pruned1=prune_strahler(x, 1)
#' plot(x)
#' plot(pruned1, lwd=3, col='blue', add=TRUE)
#' plot(pruned12, lwd=3, col='red', add=TRUE)
prune_strahler<-function(x, orderstoprune=1:2, ...) {
  tryCatch(
    prune_vertices(x, which(strahler_order(x)$points %in% orderstoprune), ...),
    error = function(c) stop(paste0("No points left after pruning. ",
                                    "Consider lowering orders to prune!"))
    )
}

#' Prune selected vertices from a neuron
#' 
#' @details uses the \code{ngraph} representation of the neuron to remove 
#'   points. It is relatively low-level function and you will probably want to 
#'   use \code{\link{subset.neuron}} or \code{\link{prune.neuron}} and friends
#'   in most cases.
#' @param x A neuron to prune
#' @param verticestoprune An integer vector describing which vertices to remove.
#'   The special signalling value of \code{NA} drops all vertices with invalid X
#'   locations.
#' @param ... Additional arguments passed to \code{\link{as.neuron.ngraph}}
#' @export
#' @seealso \code{\link{as.neuron.ngraph}}, \code{\link{subset.neuron}},
#'   \code{\link{prune.neuron}}
prune_vertices<-function(x, verticestoprune, ...) {
  g=as.ngraph(x)
  if(length(verticestoprune)==1 && is.na(verticestoprune)) {
    verticestoprune=which(!is.finite(x$d$X))
  }
  dg=igraph::delete.vertices(g, verticestoprune)
  # delete.vertices will return an igraph
  as.neuron(as.ngraph(dg), ...)
}

# Construct EdgeList matrix with start and end points from SegList
#
# @param SegList from a \code{neuron}
# @return A 2 column matrix, \code{cbind(starts,ends)}
# @export
EdgeListFromSegList<-function(SegList){
  lsl=sapply(SegList,length)
  sl=SegList[lsl>1]
  lsl=lsl[lsl>1]
  ends=unlist(lapply(sl,function(x) x[-1]))
  starts=unlist(lapply(sl,function(x) x[-length(x)]))
  cbind(starts,ends)
}
