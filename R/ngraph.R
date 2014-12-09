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
#'   graph. See \code{\link[igraph]{attributes}} for details.
#' @return an \code{igraph} object with additional class \code{ngraph}, having a
#'   vertex for each entry in vertexlabels, each vertex having a \code{label} 
#'   attribute. All vertices are included whether connected or not.
#' @family neuron
#' @seealso \code{\link{igraph}}, \code{\link[igraph]{attributes}}
#' @export
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
      xyz=xyz[,1:3]
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

as.ngraph.igraph<-function(x, directed=TRUE, root, mode=c('out','in'), ...){
  if(inherits(x,'ngraph'))
    if(is.directed(x)==directed) return(x)
  
  if(is.directed(x) && !directed) x=as.undirected(x, ...)
  else if(!is.directed(x) && directed) x=as.directed.usingroot(x, root, mode=mode, ...)
  
  if(!inherits(x,'ngraph')){
    class(x)=c("ngraph",class(x))
  }
  x
}

as.directed.usingroot<-function(g, root, mode=c('out','in')){
  mode=match.arg(mode)
  # make a directed graph _keeping any attributes_
  if(igraph::is.directed(g))
    dg=igraph::as.directed(g,mode='arbitrary')
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
#' @seealso \code{\link[igraph]{diameter}}, \code{\link[igraph]{shortest.paths}}
#' @export
#' @examples
#' plot3d(Cell07PNs[[1]])
#' plot3d(spine(Cell07PNs[[1]]), lwd=4, col='black')
#' # just extract length
#' spine(Cell07PNs[[1]], LengthOnly=TRUE)
#' # same result since StartPoint is included in longest path
#' spine(Cell07PNs[[1]], LengthOnly=TRUE, UseStartPoint=TRUE)
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
#' @param weights Whether to include the original segment lengths as weights on
#'   the graph.
#' @param exclude.isolated Whether to eliminated isolated nodes
#' @param include.xyz Whether to include 3d location as vertex attribute
#' @return \code{igraph} object containing only nodes of neuron keeping original
#'   labels (\code{x$d$PointNo} => \code{V(g)$label}) and vertex indices 
#'   (\code{1:nrow(x$d)} => \code{V(g)$vid)}.
segmentgraph<-function(x, weights=TRUE, exclude.isolated=FALSE, include.xyz=FALSE){
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
  # convert from original vertex ids to vids of reduced graph
  elred=match(t(el),all_nodes)
  
  if(weights){
    weights=seglengths(x, all=TRUE)
    g=add.edges(g, elred, weight=weights)
  } else {
    g=add.edges(g, elred)
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
