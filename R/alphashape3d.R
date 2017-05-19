#' @description \code{as.mesh3d.ashape3d} converts an 
#'   \code{alphashape3d::ashape3d} object into a nat/rgl compatible
#'   \code{mesh3d} surface
#'   
#' @details An \href{https://en.wikipedia.org/wiki/Alpha_shape}{alpha shape} is 
#'   a generalisation of a convex hull enclosing a set of points. Unlike a 
#'   convex hull, the resultant surface can be partly concave allowing the 
#'   surface to more closely follow the set of points. The parameter alpha 
#'   
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
#' clear3d()
#' wire3d(kcs20.mesh)
#' plot3d(kcs20, col=type, lwd=2)
#' }
as.mesh3d.ashape3d <- function(x, ...) {
  if (length(x$alpha) > 1)
    stop("I don't know how to handle ashape3d objects with >1 alpha value")
  iAlpha = 1

  # from help for ashape3d
  # for each alpha, a value (0, 1, 2 or 3) indicating, respectively, that the
  # triangle is not in the alpha-shape or it is interior, regular or singular
  # (columns 9 to last)

  # Pick the rows for which the triangle is regular or singular
  selrows = x$triang[, 8 + iAlpha] >= 2
  tr <- x$triang[selrows, c("tr1", "tr2", "tr3")]

  rgl::tmesh3d(
    vertices = t(x$x),
    indices = t(tr),
    homogeneous = FALSE,
    ...
  )
}
