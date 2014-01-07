#' neuron: class to represent traced neurons
#'  
#' neuron objects consist of a list containing multiple fields describing the
#' 3D location and connectivity of points in a traced neuron. The critical fields
#' of a neuron, n, are n$d which contains a dataframe in SWC format and 
#' n$SegList which contains a representation of the neuron's topology used for
#' most internal calculations.  
#' Useful functions include
#' plot.neuron
#' plot3d.neuron
#' write.neuron
#' read.neuron 
#' @name neuron
#' @family neuron
#' @seealso neuronlist
is.neuron<-function(n,Strict=FALSE) {
  # If Strict is FALSE will also return TRUE
  # if n is a list which looks like a neuron
  inherits(n,"neuron") ||
    (!Strict && is.list(n) && !is.null(n$SegList))
}

#' Arithmetic for neuron coordinates
#'
#' If x is one number or 4-vector, multiply xyz and diameter by that
#' If x is a 3-vector, multiply xyz only
#' TODO Figure out how to document arithemtic functions in one go
#' @param n a neuron
#' @param x (a numeric vector to multiply neuron coords in neuron)
#' @return modified neuron
#' @export
#' @examples
#' n1<-MyNeurons[[1]]*2
#' n2<-MyNeurons[[1]]*c(2,2,2,2)
#' stopifnot(all.equal(n1,n2))
#' n3<-MyNeurons[[1]]*c(2,2,4)
`*.neuron` <- function(n,x) {
  # TODO look into S3 generics for this functionality
  
  nd=n$d[,c("X","Y","Z","W")]
  stopifnot(is.numeric(x))
  lx=length(x)
  if(lx==1) nd[,-4]=nd[,-4]*x
  else if(lx%in%c(3,4)) nd[,1:lx]=t(t(nd[,1:lx])*x)
  else stop("expects a numeric vector of length 1, 3 or 4")
  n$d[,colnames(nd)]=nd
  n
}

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

`-.neuron` <- function(n,x) n+(-x)
`/.neuron` <- function(n,x) n*(1/x)

#' Divide neuron coords by a factor (and optionally center)
#'
#' Note that if scale=TRUE, the neuron will be rescaled to unit sd in each axis
#' likewise if center=TRUE, the neuron will be centred around the axis means
#' @param scale 3-vector used to divide x,y,z coords
#' @param center 3-vector to subtract from x,y,z coords
#' @return neuron with scaled coordinates
#' @export
#' @seealso \code{\link{scale.default}}
#' @examples
#' n1.scaledown=scale(MyNeurons[[1]],c(2,2,3))
#' n1.scaleup=scale(MyNeurons[[1]],1/c(2,2,3))
scale.neuron<-function(n,scale,center=F){
  d=xyzmatrix(n)
  ds=scale(d,scale=scale,center=center)
  n$d[,colnames(d)]=ds
  n
}

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

as.neuron<-function(n){
  if(is.null(n)) return (NULL)
  if(!is.neuron(n,Strict=TRUE)) class(n)=c("neuron",class(n))
  n
}
