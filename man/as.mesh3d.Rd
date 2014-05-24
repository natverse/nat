\name{as.mesh3d}
\alias{as.mesh3d}
\alias{as.mesh3d.hxsurf}
\title{Convert an object to an rgl mesh3d}
\usage{
as.mesh3d(x, ...)

\method{as.mesh3d}{hxsurf}(x, Regions = NULL, material = NULL, ...)
}
\arguments{
  \item{x}{Object to convert to mesh3d}

  \item{...}{Additional arguments for methods}

  \item{Regions}{Character vector or regions to select from
  \code{hxsurf} object}

  \item{material}{rgl materials such as \code{color}}
}
\description{
Note that this provides a link to the Rvcg package
}
\seealso{
\code{\link[rgl]{tmesh3d}}

Other hxsurf: \code{\link{hxsurf}},
\code{\link{read.hxsurf}}; \code{\link{plot3d.hxsurf}};
\code{\link{subset.hxsurf}}; \code{\link{write.hxsurf}}
}

