\name{plot3d.dotprops}
\alias{plot3d.dotprops}
\title{3D plots of dotprops objects using rgl package}
\usage{
\method{plot3d}{dotprops}(x, scalevecs = 1, alpharange = NULL,
  PlotPoints = FALSE, PlotVectors = TRUE, UseAlpha = FALSE, ...)
}
\arguments{
  \item{x}{A dotprops object}

  \item{scalevecs}{Factor by which to scale unit vectors
  (numeric, default: 1.0)}

  \item{alpharange}{Restrict plotting to points with
  \code{alpha} values in this range to plot (default: null
  => all points). See \code{\link{dotprops}} for definition
  of \code{alpha}.}

  \item{PlotPoints,PlotVectors}{Whether to plot points
  and/or tangent vectors (logical, default: tangent vectors
  only)}

  \item{UseAlpha}{Whether to scale tangent vector length by
  the value of \code{alpha}}

  \item{...}{Additional arguments passed to \code{points3d}
  and/or \code{segments3d}}
}
\value{
invisible list of results of rgl plotting commands
}
\description{
3D plots of dotprops objects using rgl package
}
\details{
Tangent vectors are plotted by \code{segments3d} and
centered on the relevant point. Points are plotted by
\code{points3d}.
}
\examples{
open3d()
plot3d(kcs20[[1]])
clear3d()
plot3d(kcs20[[1]],col='red')
clear3d()
plot3d(kcs20[[1]],col='red',lwd=2)
plot3d(kcs20[[2]],col='green',lwd=2)
}
\seealso{
\code{\link{dotprops}, \link[rgl]{plot3d},
\link[rgl]{points3d}, \link[rgl]{segments3d}}
}

