#' Return root, end, or branchpoints of an igraph object
#'
#' @details This function underlies \code{\link{rootpoints.igraph}} methods and
#'   friends. It is conceived of as slightly lower level and end users would
#'   normally use the \code{rootpoints} methods.
#'
#'   \code{graph.nodes} should work for any \code{\link{igraph}} object
#'   (including \code{\link{ngraph}} objects, which inherit from \code{igraph}).
#'   However the graph must be directed in order to return a root point
#' @param x An \code{\link{ngraph}} or raw \code{\link{igraph}} object
#' @param type one of root, end (which includes root) or branch
#' @param original.ids Use named attribute to return original vertex ids (when
#'   available). Set to FALSE when this is not desired.
#' @param exclude.isolated Do not count isolated vertices as root/end points
#'   (default)
#' @importFrom igraph V degree get.vertex.attribute
#' @export
#' @seealso \code{\link{rootpoints}}, \code{\link{ngraph}}
#' @examples
#' ng=as.ngraph(Cell07PNs[[1]])
#' # set some arbitrary vertex identifiers
#' igraph::vertex_attr(ng, 'name') <-sample(500, nvertices(ng))
#' # return those identifiers
#' graph.nodes(ng, type = 'end')
#' # ... or raw vertex indices
#' graph.nodes(ng,type = 'end', original.ids = FALSE)
graph.nodes<-function(x, type=c('root','end','branch'), original.ids='name',
                      exclude.isolated=TRUE){
  type=match.arg(type)
  if(type=='root' && !is.directed(x))
    stop("Cannot establish root points for undirected graph")
  
  # root points are those without incoming edges
  selected = if(type=='root') degree(x, mode='in')==0
  else if(type=='end') degree(x)<=1
  else if(type=='branch') degree(x)>2
  
  vertex_idxs=which(selected)
  if(type!='branch' && exclude.isolated) {
    # only include vertex_ids with connections
    vertex_idxs=vertex_idxs[degree(x, vertex_idxs)>0]
  }
  
  if(is.character(original.ids))
    vertex_names=get.vertex.attribute(x, original.ids, index=vertex_idxs)
  if(!is.character(original.ids) || is.null(vertex_names))
    as.integer(vertex_idxs)
  else
    vertex_names
}

#' Return the root, branch, or end points of a neuron or graph
#'
#' @description \code{rootpoints} returns the root point(s) (one per tree, often
#'   the soma).
#' @details A neuron may have multiple subtrees and therefore multiple roots. At
#'   present there is discrepancy between the \code{*.neuron} and
#'   \code{*.igraph} methods. For \code{neuron}s we return the node indices, for
#'   \code{igraph}/\code{ngraph} objects the node identifiers (aka
#'   names/PointNo)
#' @param x Neuron or other object (e.g. \code{igraph}) which might have roots
#' @param ... Further arguments passed to methods (for \code{\link{ngraph}} or
#'   \code{igraph} objects eventually \code{\link{graph.nodes}})).
#' @return FIXME Raw indices (in range 1:N) of vertices when \code{x} is a
#'   neuron, integer point identifier (aka PointNo) otherwise.
#' @export
#' @seealso \code{\link{graph.nodes}}, \code{\link{ngraph}}
#' @examples
#' rootpoints(Cell07PNs[[1]])
#' endpoints(Cell07PNs[[1]])
rootpoints<-function (x, ...)
  UseMethod("rootpoints")

#' @rdname rootpoints
#' @export
rootpoints.default<-function(x, ...) rootpoints(as.ngraph(x), ...)

#' @rdname rootpoints
#' @export
#' @param subtrees Integer index of the fully connected subtree in 
#'   \code{x$SubTrees}. Only applicable when a \code{neuron} consists of 
#'   multiple unconnected subtrees.
#' @family neuron
rootpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(x$StartPoint)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else sapply(x$SubTrees[subtrees], function(y) y[[1]][1])
}

#' @rdname rootpoints
#' @export
rootpoints.igraph<-function(x, ...) graph.nodes(x, type='root', ...)

#' @description \code{branchpoints} returns the branch points.
#' @export
#' @rdname rootpoints
#' @aliases branchpoints
branchpoints<-function (x, ...)
  UseMethod("branchpoints")

#' @rdname rootpoints
#' @export
branchpoints.default<-function(x, ...) branchpoints(as.ngraph(x), ...)

#' @rdname rootpoints
#' @details \code{branchpoints.neuron} returns a list if more than one subtree
#'   is specified
#' @export
branchpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(x$BranchPoints)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else lapply(x$SubTrees[subtrees],
              function(x) branchpoints(as.ngraph(x)))
}

#' @rdname rootpoints
#' @export
branchpoints.igraph<-function(x, ...) graph.nodes(x, type='branch', ...)

#' @description \code{endpoints} returns the end points (aka leaf nodes); the
#'   root point will be returned if it also a leaf node.
#' @rdname rootpoints
#' @export
endpoints<-function (x, ...) UseMethod("endpoints")

#' @rdname rootpoints
#' @export
endpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(endpoints=x$EndPoints)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else lapply(x$SubTrees[subtrees],
              function(x) endpoints(as.ngraph(x)))
}

#' @rdname rootpoints
#' @export
endpoints.igraph<-function(x, ...) graph.nodes(x, type='end', ...)

#' @rdname rootpoints
#' @export
endpoints.default<-function(x, ...) endpoints(as.ngraph(x), ...)
