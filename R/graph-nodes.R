#' Return root, end, or branchpoints of an igraph object
#' 
#' @details Note that the graph must be directed in order to return a root point
#' @param x An igraph object
#' @param type one of root, end (which includes root) or branch
#' @param original.ids Use named attribute to return original vertex ids (when 
#'   available). Set to FALSE when this is not desired.
#' @param exclude.isolated Do not count isolated vertices as root points 
#'   (default)
#' @importFrom igraph V degree get.vertex.attribute
graph.nodes<-function(x, type=c('root','end','branch'), original.ids='label',
                      exclude.isolated=TRUE){
  type=match.arg(type)
  if(type=='root' && !is.directed(x))
    stop("Cannot establish root points for undirected graph")
  
  # root points are those without incoming edges
  selected = if(type=='root') degree(x,mode='in')==0
  else if(type=='end') degree(x)==1
  else if(type=='branch') degree(x)>2
  
  vertex_ids=which(selected)
  if(type=='root' && exclude.isolated) {
    # only include vertex_ids with connections
    vertex_ids=vertex_ids[degree(x,vertex_ids)>0]
  }
  
  if(is.character(original.ids))
    vertex_names=get.vertex.attribute(x,original.ids,index=vertex_ids)
  if(!is.character(original.ids) || is.null(vertex_names))
    as.integer(vertex_ids)
  else
    vertex_names
}

#' Return the root or branch points of a neuron or graph
#'
#' A neuron may have multiple subtrees and therefore multiple roots
#' @param x Neuron or other object which might have roots
#' @param ... Further arguments passed to methods
#' @return Integer point number of root/branch point
#' @export
rootpoints<-function (x, ...)
  UseMethod("rootpoints")

#' @rdname rootpoints
#' @export
rootpoints.default<-function(x, ...) rootpoints(as.ngraph(x), ...)

#' @rdname rootpoints
#' @method rootpoints neuron
#' @export
#' @param subtrees Integer index of the fully connected subtree in
#'   \code{x$SubTrees}. Only applicable when a \code{neuron} consists of
#'   multiple unconnected subtrees.
rootpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(x$StartPoint)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else sapply(x$SubTrees[subtrees], function(y) y[[1]][1])
}

#' @rdname rootpoints
#' @method rootpoints igraph
#' @export
rootpoints.igraph<-function(x, ...) graph.nodes(x, type='root', ...)

#' Return the branchpoints of a neuron or graph
#' @export
#' @rdname rootpoints
#' @aliases branchpoints
branchpoints<-function (x, ...)
  UseMethod("branchpoints")

#' @rdname rootpoints
#' @export
#' @method branchpoints default
branchpoints.default<-function(x, ...) branchpoints(as.ngraph(x), ...)

#' @rdname rootpoints
#' @details \code{branchpoints.neuron} returns a list if more than one subtree is
#'   specified
#' @export
#' @method branchpoints neuron
branchpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(x$BranchPoints)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else lapply(x$SubTrees[subtrees],
              function(x) branchpoints(as.ngraph(x)))
}

#' @rdname rootpoints
#' @method branchpoints igraph
#' @export
branchpoints.igraph<-function(x, ...) graph.nodes(x, type='branch', ...)

#' @rdname rootpoints
#' @export
endpoints<-function (x, ...) UseMethod("endpoints")

#' @rdname rootpoints
#' @method endpoints neuron
#' @export
endpoints.neuron<-function(x, subtrees=1, ...){
  if(isTRUE(subtrees==1)) return(endpoints=x$EndPoints)
  nTrees=ifelse(is.null(x$nTrees),1,x$nTrees)
  if(any(subtrees>nTrees)) stop("neuron only has ",nTrees," subtrees")
  else lapply(x$SubTrees[subtrees],
              function(x) endpoints(as.ngraph(x)))
}

#' @rdname rootpoints
#' @method endpoints igraph
#' @export
endpoints.igraph<-function(x, ...) graph.nodes(x, type='end', ...)

#' @rdname rootpoints
#' @export
#' @method endpoints default
endpoints.default<-function(x, ...) endpoints(as.ngraph(x), ...)
