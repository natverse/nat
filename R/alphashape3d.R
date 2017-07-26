#' @description \code{as.mesh3d.ashape3d} converts an 
#'   \code{alphashape3d::ashape3d} object into a nat/rgl compatible
#'   \code{mesh3d} surface
#'   
#' @details An \href{https://en.wikipedia.org/wiki/Alpha_shape}{alpha shape} is 
#'   a generalisation of a convex hull enclosing a set of points. Unlike a 
#'   convex hull, the resultant surface can be partly concave allowing the 
#'   surface to more closely follow the set of points.
#'
#'   In this implementation, the parameter alpha is a scale factor with units of
#'   length that defines a spatial domain. When alpha is larger the alpha shape
#'   approaches the convex hull; when alpha is smaller the alpha shape has a
#'   greater number of faces / vertices i.e. it follows the points more closely.
#' @param tri_to_keep Which alphashape triangles to keep (expert use only - see
#'   \code{triang} entry in \bold{Value} section of
#'   \code{\link[alphashape3d]{ashape3d}} docs for details.)
#' @return a \code{\link[rgl]{mesh3d}} object which can be plotted and
#'   manipulated using \code{\link{rgl}} and \code{nat} packages.
#' @export
#' 
#' @name as.mesh3d
#' @seealso \code{\link[alphashape3d]{ashape3d}}, \code{\link[rgl]{mesh3d}}
#' @examples 
#' \donttest{
#' library(alphashape3d)
#' kcs20.a=ashape3d(xyzmatrix(kcs20), alpha = 10)
#' plot(kcs20.a)
#' 
#' # convert to mesh3d
#' kcs20.mesh=as.mesh3d(kcs20.a)
#'
#' # check that all points are inside mesh
#' all(pointsinside(kcs20, kcs20.mesh))
#' # and show that we can also use the alphashape directly
#' all(pointsinside(kcs20, kcs20.a))
#'
#' clear3d()
#' wire3d(kcs20.mesh)
#' plot3d(kcs20, col=type, lwd=2)
#' }
as.mesh3d.ashape3d <- function(x, tri_to_keep=2L, ...) {
   if (length(x$alpha) > 1)
    stop("I don't know how to handle ashape3d objects with >1 alpha value")
  iAlpha = 1

  if(!all(tri_to_keep %in% 0:3))
    stop("tri_to_keep must contain values in the range 0:3 only!")
  # from help for ashape3d
  # for each alpha, a value (0, 1, 2 or 3) indicating, respectively, that the
  # triangle is not in the alpha-shape or it is interior, regular or singular
  # (columns 9 to last)

  # Pick the rows for which the triangle is regular or singular
  selrows = x$triang[, 8 + iAlpha] %in% tri_to_keep
  tr <- x$triang[selrows, c("tr1", "tr2", "tr3")]

  m=rgl::tmesh3d(
    vertices = t(x$x),
    indices = t(tr),
    homogeneous = FALSE,
    ...
  )

  # now let's fix triangle order
  # calculate face normals for alphashape
  sn=alphashape3d::surfaceNormals(x, indexAlpha = iAlpha)
  # ... and new mesh3d object

  fn=facenormals(m)
  # calculate dot products between the two normals
  dp=dotprod(fn, sn$normals)

  # flip the vertex order when normals are in opposite directions
  m$it[,dp<0]=m$it[c(1,3,2),dp<0]
  m
}

# cross product of two Nx3 matrices (where each row is a 3D vector)
xprodm <- function(u, v) {
  cbind(u[, 2] * v[, 3] - u[, 3] * v[, 2],
        u[, 3] * v[, 1] - u[, 1] * v[, 3],
        u[, 1] * v[, 2] - u[, 2] * v[, 1])
}

# face normals of a trimesh
facenormals <- function(x, ...) {
  v=xyzmatrix(x)
  if(!length(x$it))
    stop("I only work for trimeshes")

  v1=v[x$it[1, ], ]
  v2=v[x$it[2, ], ]
  v3=v[x$it[3, ], ]
  xprodm(v1-v3, v2-v1)
}
