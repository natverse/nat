#' ngraph: a graph to encode a neuron's connectivity
#' 
#' @details We make the following assumptions about neurons coming in
#' \itemize{ 
#'   \item They have an integer vertex label that need not start from 1 and that
#'   may have gaps 
#'   \item The edge list which defines connectivity specifies edges using pairs
#'   of vertex labels, _not_ raw vertex ids.
#' }
#' @details We make no attempt to determine the root points at this stage.
#' @details The raw vertex ids in the graph will be in the order of vertexlabels
#'   and can therefore be used to index a block of vertex coordinates. The
#'   verexlabels will be stored using the vertex attribute \code{label}
#' @details When the graph is directed (default) the edges will be from the root
#'   to the other tips of the neuron.
#' @param el A two columm matrix (start, end) defining edges
#' @param vertexlabels Integer labels for graph - the edge list is specified 
#'   using these labels.
#' @param xyz 3D coordinates of vertices (optional, Nx3 matrix)
#' @param directed Whether the resultant graph should be directed (default TRUE)
#' @param graph.attributes List of named attributes to be added to the graph
#' @return an \code{igraph} object with additional class \code{ngraph}, having a
#'   vertex for each entry in vertexlabels, each vertex having a \code{label}
#'   attribute. All vertices are included whether connected or not.
#' @importFrom igraph V set.graph.attribute graph
#' @family neuron
#' @seealso \code{\link{igraph}}
ngraph<-function(el, vertexlabels, xyz=NULL, directed=TRUE, 
                 graph.attributes=NULL){
  if(any(duplicated(vertexlabels))) stop("Vertex labels must be unique!")
  # now translate edges into raw vertex_ids
  rawel=match(t(el), vertexlabels)
  g=igraph::graph(rawel, n=length(vertexlabels), directed=directed)
  igraph::V(g)$label=vertexlabels
  if(!is.null(xyz)) xyzmatrix(g)<-xyz
  for(n in names(graph.attributes)){
    g=igraph::set.graph.attribute(g,name=n,value=graph.attributes[[g]])
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
