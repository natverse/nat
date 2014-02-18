\name{im3d}
\alias{im3d}
\title{Construct an im3d object representing 3D image data, densities etc}
\usage{
im3d(x = numeric(0), dims = dim(x), voxdims = NULL, origin = NULL,
  BoundingBox = NULL, bounds = NULL)
}
\arguments{
  \item{x}{The object to turn into an im3d}

  \item{dims}{The dimensions of the image array - may be
  overridden when constructing an im3d in which the data
  block has been omitted}

  \item{voxdims}{The voxel dimensions}

  \item{origin}{the location (or centre) of the first
  voxel}

  \item{BoundingBox,bounds}{Physical extent of image. See
  \code{\link{boundingbox}} details for the distinction.}
}
\value{
An array with additional class \code{im3d}
}
\description{
Construct an im3d object representing 3D image data,
densities etc
}
\details{
We follow Amira's convention of setting the bounding box
equal to voxel dimension (rather than 0) for any dimension
with only 1 voxel.
}

