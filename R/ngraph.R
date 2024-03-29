#' ngraph: a graph to encode a neuron's connectivity
#'
#' @description the \code{ngraph} class contains a (completely general) graph
#'   representation of a neuron's connectivity in an \code{igraph} object. It
#'   may additionally contain vertex name or position data. See Connectivity
#'   section.
#'
#'   \code{ngraph()} creates an ngraph from edge and vertex information.
#' @section Connectivity: We make the following assumptions about neurons coming
#'   in
#'
#'   \itemize{
#'
#'   \item They have an integer vertex name that need not start from 1 and that
#'   may have gaps. This is analogous to the PointNo field of the core data
#'   block of \code{\link{neuron}} objects.
#'
#'   \item The edge list that defines connectivity specifies those edges using
#'   pairs of vertex names, _not_ raw vertex indices.
#'
#'   }
#'
#'   We make no attempt to determine the root points at this stage.
#'
#'   The raw vertex ids in the graph will be in the order of vertexnames and can
#'   therefore be used to index a block of vertex coordinates. The vertexnames
#'   will be stored using the vertex attribute \code{name}. The underlying
#'   igraph class allows nodes to be specified by their name. This provides a
#'   convenient way to define nodes in an ngraph object by the numeric
#'   identifier encoded by the PointNo field of the corresponding
#'   \code{\link{neuron}}.
#'
#'   When the graph is directed (default) the edges will be from the root to the
#'   other tips of the neuron.
#' @section Morphology: The morphology of the neuron is encoded by the
#'   combination of connectivity information (i.e. the graph) and spatial data
#'   encoded as the 3D position and diameter of each vertex. Position
#'   information is stored as vertex attributes X, Y, and Z.
#' @param el A two column matrix (start, end) defining edges. \code{start} means
#'   closer to the root (soma) of the neuron.
#' @param vertexnames Integer names for graph nodes - the edge list is specified
#'   using these names (see details).
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
#'   vertex for each entry in vertexnames, each vertex having a \code{label}
#'   attribute. All vertices are included whether connected or not.
#' @family neuron
#' @seealso \code{\link{igraph}}, \code{\link[igraph]{set.vertex.attribute}},
#'   \code{\link{subset.neuron}} for example of graph-based manipulation of a
#'   neuron, \code{\link{plot3d.ngraph}}
#' @export
#' @importFrom igraph V<-
#' @examples
#' n=Cell07PNs[[1]]
#' g=as.ngraph(n)
#' library(igraph)
#' # check that vertex attributes of graph match X position
#' all.equal(V(g)$X, n$d$X)
#'
#' # Use 3D segment lengths as edge length of graph
#' gw=as.ngraph(n, weights=TRUE)
#' # find longest path across graph
#' d=get.diameter(gw)
#' # make a new neuron using the longest path
#' gw_spine=as.neuron(induced.subgraph(gw, d))
#' # make a new neuron containing all nodes except those in longest path
#' gw_antispine=as.neuron(delete.vertices(gw, d))
#'
#' # note use of bounding box of original neuron to set plot axes
#' plot(gw_spine, col='red', boundingbox=boundingbox(n))
#' plot(gw_antispine, col='blue', add=TRUE)
ngraph<-function(el, vertexnames, xyz=NULL, diam=NULL, directed=TRUE,
                 weights=FALSE, vertex.attributes=NULL, graph.attributes=NULL){
  if(any(duplicated(vertexnames))) stop("Vertex names must be unique!")
  # now translate edges into raw vertex_ids
  rawel=match(t(el), vertexnames)
  
  if(any(is.na(rawel))){
    stop(sum(is.na(rawel)), "edges in el do not reference valid vertexnames!")
  }
  if(isTRUE(weights) && !is.null(xyz)){
    # rawel is no longer a matrix
    rawel.mat=matrix(rawel, nrow=2)
    starts=rawel.mat[1,]
    stops=rawel.mat[2,]
    # nb drop = FALSE to ensure that we always have a matrix
    vecs=xyz[stops, , drop=FALSE] - xyz[starts, , drop=FALSE]
    weights=sqrt(rowSums(vecs*vecs))
  }
  g=igraph::graph(rawel, n=length(vertexnames), directed=directed)
  igraph::V(g)$name=vertexnames
  if(is.numeric(weights))
    igraph::E(g)$weight=weights
  if(!is.null(xyz)) {
    if(ncol(xyz)==4 && is.null(diam)){
      diam=xyz[,4]
      xyz=xyz[, 1:3, drop=FALSE]
    }
    xyzmatrix(g)<-xyz
  }
  if(!is.null(diam)) igraph::V(g)$diam=diam
  for(n in names(vertex.attributes)){
    g=igraph::set.vertex.attribute(g, name=n,value=vertex.attributes[[n]])
  }
  for(n in names(graph.attributes)){
    g=igraph::set.graph.attribute(g, name=n,value=graph.attributes[[n]])
  }
  class(g)=c("ngraph", class(g))
  g
}

#' @description \code{as.ngraph} converts an object to an ngraph
#' 
#' @param x Object to convert (see method descriptions)
#' @param ... Arguments passed to methods
#' @export
#' @rdname ngraph
as.ngraph<-function(x, ...) UseMethod('as.ngraph')

#' @export
as.ngraph.ngraph<-function(x, ...) x

#' @description \code{as.ngraph.dataframe} construct ngraph from a data.frame 
#'   containing SWC format data
#' @export
#' @rdname ngraph
as.ngraph.data.frame<-function(x, directed=TRUE, ...){
  el=x[x$Parent!=-1,c("Parent","PointNo")]
  ngraph(data.matrix(el), x$PointNo, directed=directed, xyz=xyzmatrix(x), diam=x$W, ...)
}

#' @description \code{as.ngraph.neuron} construct ngraph from a neuron
#' @rdname ngraph
#' @export
#' @param method Whether to use the swc data (x$d) or the seglist to define 
#'   neuronal connectivity to generate graph.
#' @details Note that the \code{as.ngraph.neuron} method \emph{always} keeps the
#'   original vertex names (a.k.a. PointNo) as read in from the original file.
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
  dfs=igraph::graph.dfs(dg, root, unreachable=FALSE, dist=TRUE, mode='all')
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
#' @details Note that when \code{UseStartPoint=FALSE}, \code{spine} will find
#'   the path between all end points (including the root if it is an end point).
#'   Since the longest path must include an end point, this is equivalent to
#'   searching the whole graph for the longest path, but considerably faster.
#' @param n the neuron to consider.
#' @param UseStartPoint Whether to use the StartPoint of the neuron (often the
#'   soma) as the starting point of the returned spine.
#' @param SpatialWeights logical indicating whether spatial distances (default)
#'   should be used to weight segments instead of weighting each edge equally.
#' @param rval Character vector indicating the return type, one of
#'   \code{'neuron'}, \code{'length'} or \code{'ids'}. See \bold{Value} section.
#' @param invert When \code{invert=TRUE} the spine is pruned away instead of
#'   being selected. This is only valid when \code{rval='neuron'} or
#'   \code{rval='ids'}.
#' @return Either \itemize{
#'
#'   \item a neuron object corresponding to the longest path \emph{or}
#'
#'   \item the length of the longest path (when \code{rval="length"}) \emph{or}
#'
#'   \item an integer vector of raw point indices (when \code{rval="ids"}).
#'
#'   }
#' @seealso \code{\link[igraph]{diameter}},
#'   \code{\link[igraph]{shortest.paths}}, \code{\link{prune_strahler}} for
#'   removing lower order branches from a neuron, \code{\link{prune}} for
#'   removing parts of a neuron by spatial criteria.
#' @export
#' @examples
#' pn.spine=spine(Cell07PNs[[1]])
#' \donttest{
#' plot3d(Cell07PNs[[1]])
#' plot3d(pn.spine, lwd=4, col='black')
#' }
#' # just extract length
#' spine(Cell07PNs[[1]], rval='length')
#' # same result since StartPoint is included in longest path
#' spine(Cell07PNs[[1]], rval='length', UseStartPoint=TRUE)
#'
#' # extract everything but the spine
#' antispine=spine(Cell07PNs[[1]], invert=TRUE)
#' \donttest{
#' plot3d(Cell07PNs[[1]])
#' plot3d(antispine, lwd=4, col='red')
#' }
#'
#' @importFrom igraph shortest.paths get.shortest.paths diameter get.diameter
#'   delete.vertices
#' @family neuron
spine <- function(n, UseStartPoint=FALSE, SpatialWeights=TRUE, invert=FALSE,
                  rval=c("neuron", "length", "ids")) {
  .verify_input_neuron(n)
  ng <- as.ngraph(n, weights=SpatialWeights)
  rval=match.arg(rval)
  if(invert && rval=="length") 
    stop("invert=TRUE is not implemented for rval='length'")
  
  start <- if(UseStartPoint) n$StartPoint else n$EndPoints
  # Find distances for longest shortest paths from given start point(s) to 
  # all end points
  lps=shortest.paths(graph = ng, start, to = n$EndPoints, mode = 'all')
  if(rval=='length') return(max(lps))
  if(!UseStartPoint && length(start)>1 && length(n$EndPoints)>1) {
    # we have a square distance matrix which is symmetric across
    # the diagonal - for consistency with igraph::diameter etc
    # consider only the lower diagonal
    lps[lower.tri(lps)]=-Inf
  }
  # index of start and end points of longest path
  wmi=arrayInd(which.max(lps), dim(lps))
  from=start[wmi[1]]
  to=n$EndPoints[wmi[2]]
  longestpath=get.shortest.paths(ng, from = from, to = to, mode = 'all')$vpath[[1]]
  
  if(rval=='ids') {
    if(invert) {
      # find complement of the spine path
      ie=setdiff(igraph::E(ng), igraph::E(ng, path=longestpath))
      # find edge matrix of that path
      edgemat=igraph::get.edges(ng, ie)
      # nb transpose and then vectorise to interleave row-wise
      return(unique(as.integer(t(edgemat))))
    } else return(as.integer(longestpath))
  }
  prune_edges(ng, edges = longestpath, invert = !invert)
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
#' @importFrom igraph make_empty_graph add.edges E
#' @export
#' @examples 
#' sg=segmentgraph(Cell07PNs[[1]])
#' str(sg)
#' library(igraph)
#' plot(sg, edge.arrow.size=.4, vertex.size=10)
segmentgraph<-function(x, weights=TRUE, segids=FALSE, exclude.isolated=FALSE, 
                       include.xyz=FALSE, reverse.edges=FALSE) {
  .verify_input_neuron(x)
  pointnos=x$d$PointNo
  sts=as.seglist(x, all=TRUE, flatten = TRUE)
  # just get head and tail of each segment
  simple_sts=topntail_list(sts)
  all_nodes=sort(unique(unlist(simple_sts)))
  # make empty graph with appropriate nodes
  g=make_empty_graph(n=length(all_nodes))
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
#' @importFrom igraph graph.bfs neighborhood V
#' @return A list containing \itemize{
#'   
#'   \item points Vector of integer Strahler orders for each point in the neuron
#'   
#'   \item segments Vector of integer Strahler orders for each segment in the 
#'   neuron }
strahler_order<-function(x){
  .verify_input_neuron(x)
  s=segmentgraph(x, weights = F)
  
  roots=rootpoints(s, original.ids=FALSE)
  if(length(roots)>1)
    stop("strahler_order not yet defined for multiple subtrees")
  # quick win for single branch neurons
  if (length(x$BranchPoints) == 0)
    return(list(points=rep(1L, nrow(x$d)),
                segments=rep(1L, length(x$SegList))))
  
  b=graph.bfs(s, root=roots, mode = 'out', unreachable=F, father=T)
  
  # find neighbours for each node
  n=neighborhood(s, 1, mode='out')
  # empty vector to hold order for each node
  so_red_nodes=integer(vcount(s))
  
  # visit nodes in reverse order of bfs traversal
  # this ensures that we always visit children before parents
  for(i in rev(b$order)) {
    # faster than setdiff
    children=n[[i]][match(n[[i]], i, 0L) == 0L]
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
  segendmat=topntail(sts)
  idxs=matrix(match(segendmat,svids), ncol=2, byrow = T)
  for(i in seq_along(sts)){
    segends=segendmat[,i]
    so_segends=so_red_nodes[idxs[i,]]
    so_orig_nodes[segends]=so_segends
    so_this_seg=min(so_segends)
    so_segs[i]=so_this_seg
    internal=sts[[i]][match(sts[[i]], segends, 0L) == 0L]
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
  .verify_input_neuron(x)
  tryCatch(
    prune_vertices(x, which(strahler_order(x)$points %in% orderstoprune), ...),
    error = function(c) stop(paste0("No points left after pruning. ",
                                    "Consider lowering orders to prune!"))
    )
}

#' Prune selected vertices or edges from a neuron
#' 
#' @description \code{prune_vertices} removes vertices from a neuron
#'   
#' @details These are relatively low-level functions and you will probably want 
#'   to use \code{\link{subset.neuron}} or \code{\link{prune.neuron}} and 
#'   friends in many cases.
#'   
#'   Note that \code{prune_vertices} and \code{prune_edges} both use \bold{raw} 
#'   vertex ids to specify the vertices/edges to be removed. If you want to use 
#'   the id in the PointNo field, then you must translate yourself (see 
#'   examples).
#'   
#'   Both \code{prune_vertices} and \code{prune_edges} first convert their input
#'   \code{x} to the \code{\link{ngraph}} representation of the neuron before
#'   removing points. The input \code{x} can therefore be in any form compatible
#'   with \code{\link{as.ngraph}} including an existing \code{ngraph}. There is 
#'   an additional requirement that the input must be compatible with 
#'   \code{\link{xyzmatrix}} if \code{invert=TRUE}.
#'   
#'   Note that the \code{edges} argument of \code{prune_edges} must specify a 
#'   path traversing a set of vertices in a valid order. However if the input is
#'   a matrix or vector the direction of each individual edge in this path is
#'   ignored. So if your neuron has edges 2->1 2->3 3->4 then an edge sequence
#'   1:3 would successfully delete 2 edges. 
#'   
#' @param x A \code{\link{neuron}} to prune. This can be any object that can be 
#'   converted by \code{\link{as.ngraph}} --- see details.
#' @param verticestoprune An integer vector describing which vertices to remove.
#' @param invert Whether to keep vertices rather than dropping them (default 
#'   FALSE).
#' @param ... Additional arguments passed to \code{\link{as.neuron.ngraph}}
#' @return A pruned \code{neuron}
#' @export
#' @seealso \code{\link{as.neuron.ngraph}}, \code{\link{subset.neuron}}, 
#'   \code{\link{prune.neuron}}
#' @examples 
#' n=prune_vertices(Cell07PNs[[1]], 1:25)
#' # original neuron
#' plot(Cell07PNs[[1]])
#' # with pruned neuron superimposed
#' plot(n, col='green', lwd=3, add=TRUE)
#' 
#' # use the PointNo field (= the original id from an SWC file)
#' n2=prune_vertices(n, match(26:30, n$d$PointNo))
prune_vertices<-function(x, verticestoprune, invert=FALSE, ...) {
  g=as.ngraph(x)
  
  # because igraph.vs sequences are atttached to a specific graph
  if(inherits(verticestoprune, "igraph.vs")) 
    verticestoprune=as.integer(verticestoprune)
  if(invert) {
    nvertices=nrow(xyzmatrix(x))
    verticestoprune=setdiff(seq_len(nvertices), verticestoprune)
  }
  dg=igraph::delete.vertices(g, verticestoprune)
  # delete.vertices will return an igraph
  as.neuron(as.ngraph(dg), ...)
}

#' @description \code{prune_edges} removes edges (and any unreferenced vertices)
#' @param edges The edges to remove. One of i) an Nx2 matrix, each row
#'   specifying a single edge defined by its \bold{raw} edge id, ii) an integer
#'   vector defining a \emph{path} of raw vertex ids or iii) an \code{igraph.es}
#'   edge sequence --- see details and the \code{P} and \code{path} arguments
#'   of \code{igraph::\link[igraph]{E}}.
#' @export
#' @rdname prune_vertices
#' @examples 
#' y=prune_edges(Cell07PNs[[1]], edges=1:25)
#' 
#' # remove the spine of a neuron
#' spine_ids=spine(Cell07PNs[[1]], rval='ids')
#' pruned=prune_edges(Cell07PNs[[1]], spine_ids)
#' 
#' # NB this is subtly different from this, which removes vertices along the
#' # spine *even* if they are part of an edge that is outside the spine.
#' pruned2=prune_vertices(Cell07PNs[[1]], spine_ids)
prune_edges<-function(x, edges, invert=FALSE, ...) {
  g=as.ngraph(x)
  dg=prune_edges_ng(g, edges, invert = invert)
  as.neuron(as.ngraph(dg), ...)
}

prune_edges_ng <- function(g, edges, invert=FALSE) {
  if(!inherits(edges, "igraph.es")){
    if(!is.numeric(edges))
      stop("I can't understand the edges you have given me!")
    if(is.matrix(edges)){
      if(ncol(edges)!=2) stop("Edge matrix must have 2 columns!")
    } else {
      edges=cbind(edges[-length(edges)], edges[-1])
    }
    edges=igraph::E(g, P = as.vector(t(edges)), directed = FALSE)
  }
  
  if(invert) edges=setdiff(igraph::E(g), edges)
  
  dg=igraph::delete.edges(g, edges = edges)
  
  # remove unreferenced vertices
  dg=igraph::delete.vertices(dg, which(igraph::degree(dg, mode='all')==0))
}

# Construct EdgeList matrix with start and end points from SegList
#
# @param SegList from a \code{neuron}
# @return A 2 column matrix, \code{cbind(starts,ends)}
# @export
EdgeListFromSegList<-function(SegList){
  if(use_natcpp()) {
    return(natcpp::c_EdgeListFromSegList(SegList))
  }
  lsl=sapply(SegList,length)
  sl=SegList[lsl>1]
  lsl=lsl[lsl>1]
  ends=unlist(lapply(sl,function(x) x[-1]))
  starts=unlist(lapply(sl,function(x) x[-length(x)]))
  cbind(starts,ends)
}

#' Prune neuron(s) within a volume defined by a 3D mesh
#'
#' @details Prune a neuron to be within, or to exclude arbour within, a 3D
#'   object that can be coerced into the mesh3d data structure
#'
#' @param x a \code{neuron} object
#' @param surf An \code{\link{hxsurf}} or \code{\link[rgl]{mesh3d}} object, or
#'   any object coercible into \code{\link[rgl]{mesh3d}} by
#'   \code{\link{as.mesh3d}}.
#' @param neuropil Character vector specifying a subset of the \code{surf}
#'   object. This is only relevant when \code{surf} is of class
#'   \code{\link{hxsurf}}. If NULL (default), then the full object given as
#'   \code{surf} will be used for the pruning.
#' @param invert Logical when \code{TRUE} indicates that points inside the mesh
#'   are kept.
#' @param ... Additional arguments for methods (eventually passed to
#'   \code{\link{prune_vertices}}) surface should be pruned.
#' @inherit prune seealso
#' @examples
#' \dontrun{
#' ### Example requires the package nat.flybrains
#' LH_arbour = prune_in_volume(x = Cell07PNs, surf = nat.flybrains::IS2NP.surf,
#'   neuropil = "LH_L", OmitFailures = TRUE)
#' }
#' @return A pruned neuron/neuronlist object
#' @seealso \code{\link{as.neuron.ngraph}}, \code{\link{subset.neuron}},
#'   \code{\link{prune.neuron}}, \code{\link{prune}}
#' @export
#' @rdname prune_in_volume
prune_in_volume <-function(x, surf, neuropil = NULL, invert = TRUE, ...) 
  UseMethod("prune_in_volume")

#' @export
#' @rdname prune_in_volume
prune_in_volume.neuron <- function(x, surf, neuropil = NULL, invert = TRUE, ...) {
  if(is.null(neuropil)){
    mesh = as.mesh3d(surf)
  } else {
    stopifnot(inherits(surf, "hxsurf"))
    mesh = as.mesh3d(subset(surf, neuropil))
  }
  v = which(pointsinside(xyzmatrix(x), surf = mesh))
  neuron = prune_vertices(x, verticestoprune=v, invert=invert, ...)
  neuron
}

#' @export
#' @rdname prune_in_volume
prune_in_volume.neuronlist <- function(x, surf, neuropil = NULL, invert = TRUE, ...){
  nlapply(x, prune_in_volume.neuron, surf = surf, neuropil = neuropil, invert = invert, ...)
}

# Checks whether the input is a neuron
# @param n an object
.verify_input_neuron <- function(n) {
  if (!("neuron" %in% class(n))) {
    msg <- "Sorry, this function works only on `neuron` objects."
    if ("neuronlist" %in% class(n))
      msg <- paste(msg, "Use `nlapply` to iterate over a neuronlist.")
    stop(msg)
  }
}
