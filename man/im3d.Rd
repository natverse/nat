\name{im3d}
\alias{im3d}
\title{Construct an im3d object representing 3D image data, densities etc}
\usage{
im3d(x = numeric(0), dims = dim(x), voxdims = NULL, origin = NULL,
  BoundingBox = NULL, bounds = NULL)
}
\arguments{
  \item{dims}{The dimensions of the image array - may be
  overridden when construcing an im3d in which the data
  block has been omitted}
}
\description{
Construct an im3d object representing 3D image data,
densities etc
}

