#' Summary statistics for neurons (e.g. cable length, number of nodes)
#' 
#' @description \code{summary.neuronlist} computes tree statistics for all the 
#'   neurons in a neuronlist object
#' @param object The neuron or neuronlist to summarise
#' @param ... For \code{summary.neuronlist} additional arguments passed on to 
#'   summary methods for individual neurons
#' @param include.attached.dataframe Whether to include the neuronlists attached
#'   metadata in the returned data.frame.
#'   
#' @export
#' @rdname summary.neuron
#' @aliases summary
#' @seealso \code{\link{seglengths}}, \code{\link[Rvcg]{vcgArea}}
#' @examples 
#' # summary for a whole neuronlist
#' summary(Cell07PNs)
#' # including the attached data.frame with additional metadata
#' head(summary(Cell07PNs, include.attached.dataframe = FALSE))
summary.neuronlist<-function(object, ..., include.attached.dataframe=FALSE) {
  l=lapply(object, summary)
  d=do.call(rbind, l)
  if(include.attached.dataframe) cbind(data.frame(object),d) else d
}

#' @description \code{summary.neuron} computes statistics for individual neurons
#' @return A \code{data.frame} summarising the tree properties of the neuron 
#'   with columns \itemize{
#'   
#'   \item{root}
#'   
#'   \item{nodes}
#'   
#'   \item{segments}
#'   
#'   \item{branchpoints}
#'   
#'   \item{endpoints}
#'   
#'   \item{cable.length}
#'   
#'   \item{nTrees}
#'   
#'   }
#' @export
#' @examples 
#' # for a single regular format neuron
#' summary(Cell07PNs[[1]])
summary.neuron<-function(object, ...) {
  data.frame(root=object$StartPoint,
             nodes=nrow(object$d),
             segments=object$NumSegs,
             branchpoints=length(object$BranchPoints),
             endpoints=length(object$EndPoints),
             cable.length=total_cable(object),
             nTrees=ifelse("nTrees" %in% names(object), object$nTrees, 1))
}

#' @export
#' @rdname summary.neuron
#' @description \code{summary.mesh3d} computes statistics including face numbers
#'   and surface area for meshes. See \code{\link[Rvcg]{vcgArea}} for details of
#'   area calculation.
summary.mesh3d <- function(object, ...) {
  inds=if(is.null(object$it)) object$ib else object$it
  area <- if(requireNamespace('Rvcg', quietly=TRUE))
    Rvcg::vcgArea(object, perface = FALSE)
  else NA
  df=data.frame(vertices=ncol(object$vb),
             faces=ncol(inds), 
             edges=length(inds),
             area=area,
             facetype=ifelse(nrow(inds)==3, "triangle", "quad"))
}

total_cable <- function(x) {
  diffs <- x$d[,c("X","Y","Z")]-x$d[match(x$d$Parent, x$d$PointNo),c("X","Y","Z")]
  sum(sqrt(rowSums(diffs*diffs)), na.rm = T)
}

#' @description \code{summary.dotprops} computes statistics for individual
#'   neurons in dotprops format. Note the \code{veclength} argument.
#' @export
#' @param veclength The vector length to assume for each segment so that a cable
#'   length estimate can be made.
#' @rdname summary.neuron
#' @examples
#' # for a single dotprops format neuron
#' summary(kcs20[[1]])
#' # specify a different estimate for the cable length associated with a single
#' # point in the neuron
#' summary(kcs20[[1]], veclength=1.2)
summary.dotprops<-function(object, veclength=1, ...) {
  data.frame(nodes=nrow(object$points), 
             cable.length=nrow(object$points)*veclength)
}

#' @export
print.neuron <- function(x, ...) {
  ntrees=if(is.null(x$nTrees)) 1 else x$nTrees
  nx=nvertices(x)
  
  cat("'neuron' with", nx, ifelse(nx==1, 'vertex', 'vertices'),
      'in', ntrees, ifelse(ntrees==1, 'tree', 'trees'))
  extraclasses=setdiff(class(x), c("neuron","list"))
  squote <- function(x) paste("'",x,"'", collapse = ', ', sep='')
  if(length(extraclasses)>0)
    cat(" and additional classes", squote(extraclasses))
  cat("\n")
}

#' @export
print.dotprops <- function(x, ...) {
  nx=nvertices(x)
  cat("'dotprops' object with", nx, ifelse(nx==1, 'vertex', 'vertices'))
  cat("\n")
}

#' @export
print.neuronlist <- function(x, ...) {
  firstclass <- function(x) class(x)[[1]]
  contents.classes <- sapply(x, firstclass, USE.NAMES = F)
  ucontents.classes <- unique(contents.classes)
  
  nc=ncol(as.data.frame(x))
  
  cat("'",firstclass(x),"'", " containing ", length(x), 
      " '",ucontents.classes,"' ",
      ifelse(length(x)==1, "object", "objects"), " ", sep=""
  )
  if(length(ucontents.classes)>1) {
    cat("of classes\n")
    tt=table(contents.classes)
    dimnames(tt)=unname(dimnames(tt))
    print(tt)
  }
  cat("and 'data.frame' with",nc,'vars ')
  cat("[", 
      format(object.size(x), units='auto', standard="SI"), 
      "]", "\n", sep="")
}

#' @export
print.neuronlistz <- function(x, ...) {
  firstclass <- function(x) class(x)[[1]]

  cat("'",firstclass(x),"'", " containing ", length(x),
      " neurons ",
      ifelse(length(x)==1, "object", "objects"), " ", sep=""
  )
  nc=ncol(as.data.frame(x))
  cat("and 'data.frame' with ",nc,' vars [',
      format(object.size(x), units='auto', standard="SI")
      , ' in RAM].\n', sep="")
  f_path=attr(x, "db")
  f_size=format(structure(file.info(f_path)$size, class='object_size'),
                units='auto',standard="SI")
  cat("Loaded on demand from '", normalizePath(f_path), "' [",
      f_size, " on disk].\n", sep="")
}
