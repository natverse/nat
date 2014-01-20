#' neuron: class to represent traced neurons
#' 
#' neuron objects consist of a list containing multiple fields describing the 3D
#' location and connectivity of points in a traced neuron. The critical fields 
#' of a neuron, n, are n$d which contains a dataframe in SWC format and 
#' n$SegList which contains a representation of the neuron's topology used for 
#' most internal calculations. Useful functions include plot.neuron 
#' plot3d.neuron write.neuron read.neuron
#' @description \code{neuron} makes a neuron object from appropriate variables.
#' @details StartPoint,BranchPoints,EndPoints are indices matching the rows of 
#'   the vertices in \code{d} \strong{not} arbitrary point numbers typically 
#'   encoded in \code{d$PointNo}.
#' @rdname neuron
#' @export neuron
#' @family neuron
#' @seealso \code{\link{neuronlist}}
#' @param d matrix of vertices and associated data in SWC format
#' @param StartPoint,BranchPoints,EndPoints Nodes of the neuron
#' @param SegList List where each element contains the vertex indices for a 
#'   single segments of the neuron, starting at root.
#' @param SubTrees List of SegLists where a neuron has multiple unconnected 
#'   trees (e.g. because the soma is not part of the graph, or because the 
#'   neuronal arbour has been cut.)
#' @param ... Additional fields to be included in neuron. Note that if these
#'   include CreatedAt, NodeName, InputFileStat or InputFileMD5, they will
#'   override fields of that name that are calculated automatically.
#' @param InputFileName Character vector with path to input file
#' @param NeuronName Character vector containing name of neuron
#' @param MD5 Logical indicating whether to calculate MD5 hash of input
#' @importFrom tools md5sum
neuron<-function(d, StartPoint, BranchPoints=integer(), EndPoints,
                 SegList, SubTrees=NULL, InputFileName=NULL, NeuronName=NULL, ...,
                 MD5=TRUE){
  
  coreFieldOrder=c("NumPoints", "StartPoint", "BranchPoints", 
               "EndPoints", "nTrees", "NumSegs", "SegList", "SubTrees","d" )
  mcl<-as.list(match.call())
  n=c(mcl,list(NumPoints=nrow(d),
             nTrees=ifelse(is.null(SubTrees),1,length(SubTrees)),
             NumSegs=length(SegList)))
  n=n[intersect(coreFieldOrder,names(n))]
  
  if(!is.null(InputFileName)){
    if(is.null(NeuronName)) NeuronName=basename(InputFileName)
    else if(is.function(NeuronName)) NeuronName=NeuronName(InputFileName)
    neuron_extra=list(NeuronName=NeuronName,
                      InputFileName=InputFileName,
                      CreatedAt=Sys.time(),
                      NodeName=Sys.info()["nodename"])
    if(file.exists(InputFileName)) {
      neuron_extra$InputFileStat=file.info(InputFileName)[1,]
      if(MD5) neuron_extra$InputFileMD5=md5sum(InputFileName)
    }
    n=c(neuron_extra,n)
  }
  dots=list(...)
  if(length(dots)) {
    n[names(dots)]=dots
  }
  as.neuron(n)
}

#' @param x A neuron or other object to test/convert
#' @description \code{is.neuron} will check if an object looks like a neuron.
#' @param Strict Whether to check class of neuron or use a more relaxed
#'   definition based on object being a list with a SegList component.
#' @export
#' @rdname neuron
is.neuron<-function(x,Strict=FALSE) {
  inherits(x,"neuron") ||
    (!Strict && is.list(x) && !is.null(x$SegList))
}

#' @description \code{as.neuron} will add class "neuron" to a neuron-like
#'   object.
#' @export
#' @rdname neuron
as.neuron<-function(x){
  if(is.null(x)) return (NULL)
  if(!is.neuron(x,Strict=TRUE)) class(x)=c("neuron",class(x))
  x
}

#' Arithmetic for neuron coordinates
#'
#' If x is a 1-vector or a 3-vector, multiply xyz only
#' If x is a 4-vector, multiply xyz and diameter by that
#' TODO Figure out how to document arithemtic functions in one go
#' @param n a neuron
#' @param x (a numeric vector to multiply neuron coords in neuron)
#' @return modified neuron
#' @export
#' @rdname neuron-arithmetic
#' @seealso neuron
#' @examples
#' n1<-Cell07PNs[[1]]*2
#' n2<-Cell07PNs[[1]]*c(2,2,2,1)
#' stopifnot(all.equal(n1,n2))
#' n3<-Cell07PNs[[1]]*c(2,2,4)
#' @method * neuron
`*.neuron` <- function(n,x) {
  # TODO use xyzmatrix
  
  nd=n$d[,c("X","Y","Z","W")]
  stopifnot(is.numeric(x))
  lx=length(x)
  if(lx==1) nd[,-4]=nd[,-4]*x
  else if(lx%in%c(3,4)) nd[,1:lx]=t(t(nd[,1:lx])*x)
  else stop("expects a numeric vector of length 1, 3 or 4")
  n$d[,colnames(nd)]=nd
  n
}

#' @method + neuron
#' @rdname neuron-arithmetic
#' @export
`+.neuron` <- function(n,x) {
  if(!is.numeric(x))
    stop("expects a numeric vector")
  nd=n$d[,c("X","Y","Z","W")]
  lx=length(x)
  if(lx==1) nd[,-4]=nd[,-4]+x
  else if(lx%in%c(3,4)) nd[,1:lx]=t(t(nd[,1:lx])+x)
  else stop("expects a numeric vector of length 1, 3 or 4")
  n$d[,colnames(nd)]=nd
  n
}

#' @method - neuron
#' @rdname neuron-arithmetic
#' @export
`-.neuron` <- function(n,x) {
  if(!missing(x))
    n+(-x)
  else {
    n*-1
  }
}

#' @method / neuron
#' @rdname neuron-arithmetic
#' @export
`/.neuron` <- function(n,x) n*(1/x)

#' Divide neuron coords by a factor (and optionally center)
#'
#' @details Note that if scale=TRUE, the neuron will be rescaled to unit sd in each axis
#' likewise if center=TRUE, the neuron will be centred around the axis means
#' @param x A neuron
#' @param center 3-vector to subtract from x,y,z coords
#' @param scale 3-vector used to divide x,y,z coords
#' @return neuron with scaled coordinates
#' @method scale neuron
#' @export
#' @seealso \code{\link{scale.default}}
#' @examples
#' n1.scaledown=scale(Cell07PNs[[1]],scale=c(2,2,3))
#' n1.scaleup=scale(Cell07PNs[[1]],scale=1/c(2,2,3))
scale.neuron<-function(x,center=FALSE,scale=FALSE){
  xyzmatrix(x)<-scale(xyzmatrix(x),scale=scale,center=center)
  x
}

#' Check equality on key fields of neuron object
#' 
#' @inheritParams base::all.equal.default
#' @param fieldsToCheck Which fields in the neuron are always check
#' @param fieldsToCheckIfPresent These fields are only checked if they are
#'   present
#' @param CheckSharedFieldsOnly Logical whether to check shared fields only 
#'   (default: FALSE)
#' @param ... additional arguments passed to \code{all.equal}
#' @method all.equal neuron
#' @export
#' @seealso \code{\link{all.equal}}
all.equal.neuron<-function(target,current,tolerance=1e-6,check.attributes=FALSE,
                           fieldsToCheck=c("NeuronName", "NumPoints", "StartPoint", "BranchPoints",
                                           "EndPoints", "NumSegs", "SegList", "d"), fieldsToCheckIfPresent="nTrees",
                           CheckSharedFieldsOnly=FALSE, ...){
  if(length(fieldsToCheck)==1 && is.na(fieldsToCheck))
    fieldsToCheck=names(current)
  
  if(!is.neuron(target) || !is.neuron(current))
    return ("target and current must both be neurons")
  fieldsInCommon=intersect(names(target),names(current))
  # figure out which of the optional fields to check are present
  fieldsToCheckIfPresent=intersect(fieldsInCommon,fieldsToCheckIfPresent)
  # and add those to the fields to check 
  fieldsToCheck=unique(c(fieldsToCheck,fieldsToCheckIfPresent))
  if(CheckSharedFieldsOnly){
    fieldsToCheck=intersect(fieldsInCommon,fieldsToCheck)
  } else{
    # check all core fields
    missingfields=setdiff(fieldsToCheck,names(current))
    if(length(missingfields)>0)
      return(paste("Current missing fields: ",missingfields))
    missingfields=setdiff(fieldsToCheck,names(target))
    if(length(missingfields)>0)
      return(paste("Target missing fields: ",missingfields))		
  }
  all.equal(target[fieldsToCheck],current[fieldsToCheck],
            tolerance=tolerance, check.attributes=check.attributes, ...)
}
