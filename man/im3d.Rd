\name{im3d}
\alias{im3d}
\title{Construct an im3d object representing 3D image data, densities etc}
\usage{
im3d(x = numeric(0), dims = NULL, voxdims = NULL, origin = NULL,
  BoundingBox = NULL, bounds = NULL, ...)
}
\arguments{
  \item{x}{The object to turn into an im3d}

  \item{dims}{The dimensions of the image array either as
  an integer vector \emph{or} as an im3d object, whose
  attributes will provide defaults for \code{dims, origin,
  BoundingBox, bounds} arguments. The default
  (\code{dims=NULL}) will result in \code{dims} being set
  to \code{x} if \code{x} is an \code{im3d} object or
  \code{dim(x)} otherwise.}

  \item{voxdims}{The voxel dimensions}

  \item{origin}{the location (or centre) of the first
  voxel}

  \item{BoundingBox,bounds}{Physical extent of image. See
  the details section of \code{\link{boundingbox}}'s help
  for the distinction.}

  \item{...}{Additional attributes such as units or
  materials}
}
\value{
An array with additional class \code{im3d}
}
\description{
\code{im3d} objects consist of a data array with attributes
defining the spatial positions at which the voxels are
located. There should always be a \code{BoundingBox}
attribute which defines the physical extent of the volume
in the same manner as the Amira 3d visualisation and
analysis software. This corresponds to the \strong{node}
centers option in the
\href{http://teem.sourceforge.net/nrrd/format.html}{NRRD
format}.
}
\details{
We follow Amira's convention of setting the bounding box
equal to voxel dimension (rather than 0) for any dimension
with only 1 voxel.
}
\seealso{
Other im3d: \code{\link{as.im3d}},
\code{\link{as.im3d.im3d}}; \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{ijkpos}},
\code{\link{ijkpos}}, \code{\link{im3d-coords}},
\code{\link{im3d-coords}}, \code{\link{xyzpos}};
\code{\link{im3d-io}}, \code{\link{read.im3d}},
\code{\link{write.im3d}}; \code{\link{imexpand.grid}};
\code{\link{imslice}}; \code{\link{origin}};
\code{\link{projection}}; \code{\link{threshold}},
\code{\link{threshold.im3d}}; \code{\link{unmask}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

