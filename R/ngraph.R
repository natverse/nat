#' ngraph: a graph to encode a neuron's connectivity
#' 
#' @description the \code{ngraph} class contains a (completely general) graph 
#'   representation of a neuron's connectivity in an \code{igraph} object. It 
#'   may additionally contain vertex label or position data. See details.
#'   
#'   \code{ngraph()} creates an ngraph from edge and vertex information.
#' @details We make the following assumptions about neurons coming in
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
#' @param el A two columm matrix (start, end) defining edges
#' @param vertexlabels Integer labels for graph - the edge list is specified 
#'   using these labels.
#' @param xyz 3D coordinates of vertices (optional, Nx3 matrix)
#' @param weights Logical value indicating whether edge weights defined by the
#'   3D distance between points should be added to graph (default \code{FALSE})
#'   \emph{or} a numeric vector of weights.
#' @param directed Whether the resultant graph should be directed (default TRUE)
#' @param graph.attributes List of named attributes to be added to the graph
#' @return an \code{igraph} object with additional class \code{ngraph}, having a
#'   vertex for each entry in vertexlabels, each vertex having a \code{label} 
#'   attribute. All vertices are included whether connected or not.
#' @family neuron
#' @seealso \code{\link{igraph}}
#' @export
ngraph<-function(el, vertexlabels, xyz=NULL, directed=TRUE, weights=FALSE,
                 graph.attributes=NULL){
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
  if(!is.null(xyz)) xyzmatrix(g)<-xyz
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
  ngraph(data.matrix(el), x$PointNo, directed=directed, xyz=xyzmatrix(x), ...)
}

#' @description \code{as.ngraph.neuron} construct ngraph from a neuron
#' @rdname ngraph
#' @export
#' @method as.ngraph neuron
#' @param method Whether to use the swc data (x$d) or the seglist to define 
#'   neuronal connectivity to generate graph.
#' @details Note that this method \emph{always} keeps the original vertex labels
#'   (a.k.a. PointNo) as read in from the original file.
as.ngraph.neuron<-function(x, directed=TRUE, method=c('swc','seglist'), ...){
  method=match.arg(method, several.ok=TRUE)
  if('swc'%in%method && !is.null(x$d$Parent) && !is.null(x$d$PointNo)){
    as.ngraph(x$d, directed=directed, ...)
  } else {
    as.ngraph(seglist2swc(x)$d, directed=directed)
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
#' @param SpatialWeights logical indicating whether spatial distances (default) 
#'   should be used to weight segments instead of weighting each edge equally.
#' @param LengthOnly logical indicating whether only the length of the longest 
#'   path should be returned (when \code{TRUE}) or whether a neuron pruned down 
#'   to the the sequence of vertices along the path should be returned 
#'   (\code{FALSE}, the default).
#' @return Either a neuron object corresponding to the longest path \emph{or}
#'   the length of the longest path when \code{LengthOnly=TRUE}).
#' @seealso \code{\link[igraph]{diameter}}
#' @export
#' @examples
#' plot3d(Cell07PNs[[1]])
#' plot3d(spine(Cell07PNs[[1]]), lwd=4, col='black')
spine <- function(n, SpatialWeights=TRUE, LengthOnly=FALSE) {
  ng <- as.ngraph(n, weights=SpatialWeights)
  if(LengthOnly) {
    diameter(ng, directed=FALSE)
  } else {
    longestpath=get.diameter(ng, directed=FALSE)
    spineGraph <- delete.vertices(ng, setdiff(V(ng), longestpath))
    as.neuron(as.ngraph(spineGraph), vertexData=n$d[V(spineGraph)$label, ])
  }
}
