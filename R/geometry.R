#' Find the coefficients of the plane equation
#' 
#' @param p A point on the plane (or N x 3 matrix of multiple points)
#' @param n vector normal to the plane (or N x 3 matrix of multiple vectors)
#' @details Both \code{p} and \code{n} can accept multiple points/vectors to 
#'   calculate many planes at once.
#' @return a matrix with 4 columns \code{a,b,c,d} where \code{ax + by + cz + d =
#'   0}
#' @family geometry
#' @export
#' @examples 
#' 
#' # Mushroom Body Entry Point - plane perpendicular to axon tract as 
#' # projection neurons enter mushroom body calyx
#' mbe=plane_coefficients(p=c(207, 102, 142), n=c(.6,-0.1,0.3))
#' \dontrun{
#' plot3d(Cell07PNs)
#' planes3d(mbe[1:3], d=mbe[4])
#' }
plane_coefficients<-function(p,n){
  # p is point, n is normal vector
  if(is.null(dim(p))) p=matrix(p,ncol=3)
  if(is.null(dim(n))) n=matrix(n,ncol=3)
  k=apply(n*p,1,sum)
  res=cbind(n,-k)
  colnames(res)=letters[1:4]
  res
}

#' Find the points on a plane that are intersected by an object
#' 
#' @param x A neuron, set of line segments or other data - see details.
#' @param plane A plane, specified by the 4 coeffients of the plane equation 
#'   (see \code{\link{plane_coefficients}})
#' @param ... Additional arguments passed to methods
#'   
#' @return A Nx3 matrix of the X,Y,Z positions of the intersections (NA when
#'   there is no intersection)
#' @export
#' @family geometry
#' @examples
#' ## Find plane coefficients
#' # point on plane
#' cent=c(250.4987, 95.73561, 140.2052)
#' # vector normal to plane
#' vec=c(0.7709581, 0.03417276, -0.411977)
#' plc=plane_coefficients(cent, vec)
#' 
#' ## intersect with plane
#' ip=intersect_plane(Cell07PNs[[1]], plc)
#' plot(Cell07PNs[[1]], WithNodes=FALSE)
#' points(ip[1], ip[2], pch=19, cex=2, col='red')
#' 
#' \dontrun{
#' plot3d(Cell07PNs[[1]], col='grey', WithNodes=FALSE)
#' spheres3d(matrix(ip, ncol=3), col='red', rad=2)
#' planes3d(plc[,1:3], d=plc[,'d'])
#' }
intersect_plane <- function(x, plane, ...) UseMethod('intersect_plane')

#' @rdname intersect_plane
#' @export
intersect_plane.default<-function(x, plane, ...){
  # the line seg is defined by two points in a 2x3nN where cols are X,Y,Z
  # and rows are the different points and 3rd dim is different line segs
  # the plane is defined the coefficients A,B,C,D
  # for  Ax + By + Cz + D = 0
  # This function will return NA if the plane does not lie between 
  # the 2 points
  dimx=dim(x)
  if(!identical(dimx[1:2], 2:3) || !length(dimx) %in% 2:3)
    stop("x must be a 2 x 3 x N array of line segments")
  
  if(identical(dimx, 2:3)){
    # only one point in 2X3 
    u=(sum(plane[1:3]*x[1,]) +plane[4])/(sum(plane[1:3]*(x[1,]-x[2,])))
    #cat("u =",u,"\n")
    if(u>1 || u<0) return(c(NA,NA,NA))
    return(x[1,]+u*(x[2,]-x[1,]))
  } else {
    # 2x3xN
    # output will have cols for different line segs, rows for each of
    # three points
    u=(apply(plane[1:3]*x[1,,],2,sum) +plane[4])/apply(plane[1:3]*(x[1,,]-x[2,,]),2,sum)
    #rval=x[1,,]+u*(x[2,,]-x[1,,])
    # nb t so that rows are points and cols are x,y,z
    rval=t(x[1,,])+apply(x[2,,]-x[1,,],1,"*",u)
    #cat("u =",u,"\n")
    rval[u>1 | u<0,]=NA
    rval
  }
}

#' @rdname intersect_plane
#' @description \code{intersect_plane.neuron} finds the place where a neuron intersection 
#' @export
#' @param closestpoint Used to define the closest hit when there are multiple
intersect_plane.neuron<-function(x, plane, closestpoint=NULL, ...){
  # consider every line in neuron (ie each point and its parent)
  # make a 3d matrix R,C,i Rows are point 1 and 2, cols are XYZ and i are
  # multiple lines

  if(nrow(x$d)<2) stop("Need at least 2 points to define a line in Neuron")
  d=x$d
  points=unique(unlist(x$SegList))
  
  PointMatrix=array(0,dim=c(2,3,length(points)-1))
  # set up the ends of the lines
  PointMatrix[2,,]=t(as.matrix(d[points[-1],c("X","Y","Z")]))
  # set up the starts of the lines
  start_indices=match(d$Parent[points[-1]], d$PointNo)
  PointMatrix[1,,]=t(as.matrix(d[start_indices ,c("X","Y","Z")]))
  rval=intersect_plane(PointMatrix, plane)
  # return any non NA rows
  rval=rval[!is.na(rval[,1]),]
  if(is.matrix(rval) && nrow(rval)>1 && !is.null(closestpoint)){
    # find the closest intersection point
    squaredist=colSums((t(rval)-closestpoint)^2)
    rval=rval[which.min(squaredist),]
  }
  rval
}
