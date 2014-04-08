\name{ind2coord}
\alias{ind2coord}
\alias{ind2coord.array}
\alias{ind2coord.default}
\alias{ind2coord.im3d}
\title{Find XYZ coords corresponding to 1D indices into a 3D image}
\usage{
ind2coord(inds, ...)

\method{ind2coord}{default}(inds, dims, voxdims, origin, ...)

\method{ind2coord}{array}(inds, voxdims = NULL, origin = NULL, ...)

\method{ind2coord}{im3d}(inds, voxdims = NULL, origin = NULL, ...)
}
\arguments{
  \item{inds}{indices into an image array (either 1D, for
  which \code{dims} must be present, or a logical array).}

  \item{...}{extra arguments passed to methods.}

  \item{dims}{dimensions of 3d image array.}

  \item{voxdims}{vector of 3 voxel dimensions (width,
  height, depth).}

  \item{origin}{the origin.}
}
\description{
Find XYZ coords corresponding to 1D indices into a 3D image
}
\seealso{
\code{\link{coord2ind}}, \code{\link{sub2ind}}
}

