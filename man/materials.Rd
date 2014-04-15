\name{materials}
\alias{materials}
\alias{materials.character}
\alias{materials.hxsurf}
\alias{materials.im3d}
\title{Extract the materials for an object}
\usage{
materials(x, ...)

\method{materials}{im3d}(x, ...)

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
Extract the materials for an object

\code{materials.character} will read the materials from an
im3d compatible image file on disk.

\code{materials.character} will read the materials from an
im3d compatible image file.
}

