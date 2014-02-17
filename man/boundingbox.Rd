\name{boundingbox}
\alias{boundingbox}
\alias{boundingbox.im3d}
\title{Get the bounding box of an im3d volume or other compatible object}
\usage{
boundingbox(x, ...)

\method{boundingbox}{im3d}(x, dims = dim(x), ...)
}
\arguments{
  \item{x}{A vector, matrix or im3d object or, for
  \code{boundingbox.character} a character vector
  specifying a file.}

  \item{...}{Additional arguments passed to methods}

  \item{dims}{The dimensions of the image array - can be
  used to override dim(x) e.g. when only the metadata for
  an im3d has been read in.}
}
\description{
Get the bounding box of an im3d volume or other compatible
object
}
\details{
The bounding box is defined as the cuboid with vertices at
the most extreme vertices of an image, \emph{when those
vertices are assumed to have a single position (sometimes
thought of as their centre) }\strong{and no physical
extent.}
}

