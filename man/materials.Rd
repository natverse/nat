\name{materials}
\alias{materials}
\alias{materials.character}
\alias{materials.default}
\alias{materials.hxsurf}
\title{Extract or set the materials for an object}
\usage{
materials(x, ...)

\method{materials}{default}(x, ...)

\method{materials}{character}(x, ...)

\method{materials}{hxsurf}(x, ...)
}
\arguments{
  \item{x}{An object in memory or, for
  \code{materials.character}, an image on disk.}

  \item{\dots}{additional parameters passed to methods
  (presently ignored)}
}
\value{
A \code{data.frame} with columns \code{name, id, col}
}
\description{
Extract or set the materials for an object

\code{materials.character} will read the materials from an
im3d compatible image file on disk.

\code{materials.character} will read the materials from an
im3d compatible image file.
}
\details{
Note that the id column will be the 1-indexed order that
the material appears in the \code{surf$Region} list for
\code{hxsurf} objects and the 0-indexed mask values for a
nrrd
}

