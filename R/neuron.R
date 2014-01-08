#' neuron: class to represent traced neurons
#' 
#' neuron objects consist of a list containing multiple fields describing the 3D
#' location and connectivity of points in a traced neuron. The critical fields 
#' of a neuron, n, are n$d which contains a dataframe in SWC format and 
#' n$SegList which contains a representation of the neuron's topology used for 
#' most internal calculations. Useful functions include plot.neuron 
#' plot3d.neuron write.neuron read.neuron
#' @rdname neuron
#' @family neuron
#' @seealso neuronlist
#' @param n A neuron
#' @description \code{is.neuron} will check if an object looks like a neuron.
#' @param Strict Whether to check class of neuron or use a more relaxed
#'   definition based on object being a list with a SegList component.
#' @export
is.neuron<-function(n,Strict=FALSE) {
  # If Strict is FALSE will also return TRUE
  # if n is a list which looks like a neuron
  inherits(n,"neuron") ||
    (!Strict && is.list(n) && !is.null(n$SegList))
}

#' @description \code{as.neuron} will add class "neuron" to a neuron-like
#'   object.
#' @export
#' @rdname neuron
as.neuron<-function(n){
  if(is.null(n)) return (NULL)
  if(!is.neuron(n,Strict=TRUE)) class(n)=c("neuron",class(n))
  n
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
}

#' Get and assign coordinates for classes containing 3d vertex data
#' 
#' @param x object containing 3d coordinates
#' @param ... additional arguments passed to methods
#' @return Nx3 matrix containing 3d coordinates
#' @export
xyzmatrix<-function(x, ...) UseMethod("xyzmatrix")

#' @method xyzmatrix default
#' @param y,z separate y and z coordinates
#' @param Transpose Whether to transpose the coordinates to 3xN matrix
#' @rdname xyzmatrix
#' @export
xyzmatrix.default<-function(x,y=NULL,z=NULL,Transpose=FALSE,...) {
  # quick function that gives a generic way to extract coords from 
  # classes that we care about and returns a matrix
  # nb unlike xyz.coords this returns a matrix (not a list)
  x=if(is.neuron(x)) x$d[,c("X","Y","Z")]
  else if(is.dotprops(x)) x$points
  else if(!is.null(z)){
    cbind(x,y,z)
  } else x
  mx=data.matrix(x)
  if(Transpose) t(mx) else mx
}

#' @description Assign xyz elements of neuron or dotprops object. Can also
#'   handle matrix like objects with columns named X,Y,Z
#' @usage xyzmatrix(x) <- value
#' @param value Nx3 matrix specifying new xyz coords
#' @return Original object with modified coords
#' @export
#' @seealso \code{\link{xyzmatrix}}
#' @rdname xyzmatrix
`xyzmatrix<-`<-function(x, value) UseMethod("xyzmatrix<-")

#' @rdname xyzmatrix
#' @method xyzmatrix<- default
#' @usage xyzmatrix(x) <- value
#' @export
#' @examples
#' n=Cell07PNs[[1]]
#' xyzmatrix(n)<-xyzmatrix(n)
#' stopifnot(isTRUE(
#'   all.equal(xyzmatrix(n),xyzmatrix(Cell07PNs[[1]]))
#' ))
`xyzmatrix<-.default`<-function(x, value){
  if(is.neuron(x)) x$d[,c("X","Y","Z")]=value
  else if(is.dotprops(x)) x$points[,c("X","Y","Z")]=value
  else if(all(c("X","Y","Z") %in% colnames(x))) x[,c("X","Y","Z")]=value
  else stop("Not a neuron or dotprops object or a matrix-like object with XYZ volnames")
  x
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
