\name{plot3d.hxsurf}
\alias{plot3d.hxsurf}
\title{Plot amira surface objects in 3d using rgl}
\usage{
\method{plot3d}{hxsurf}(x, materials = x$RegionList, col = NULL, ...)
}
\arguments{
  \item{x}{An hxsurf surface object}

  \item{materials}{Character vector naming materials to
  plot (defaults to all materials in x)}

  \item{col}{Character vector specifying colors for the
  materials, or a function that will be called with the
  number of materials to plot. When \code{NULL} (default)
  will use meterial colours defined in Amira (if
  available), or \code{rainbow} otherwise.}

  \item{...}{Additional arguments passed to}
}
\description{
Plot amira surface objects in 3d using rgl
}
\seealso{
\code{\link{read.hxsurf}}
}

