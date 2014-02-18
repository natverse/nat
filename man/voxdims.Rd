\name{voxdims}
\alias{voxdims}
\alias{voxdims.default}
\title{Return voxel dimensions of an object}
\usage{
voxdims(x, ...)

\method{voxdims}{default}(x, dims, ...)
}
\arguments{
  \item{x}{An \code{im3d} object with associated voxel
  dimensions or a 2 x 3 BoundingBox \code{matrix}.}

  \item{...}{Additional arguments for methods}

  \item{dims}{The number of voxels in each dimension when x
  is a BoundingBox matrix.}
}
\value{
A numeric vector of length 3, NA when NULL
}
\description{
Return voxel dimensions of an object
}
\seealso{
\code{\link{boundingbox}}

Other im3d: \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}}
}

