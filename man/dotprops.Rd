\name{is.dotprops}
\alias{as.dotprops}
\alias{dotprops}
\alias{dotprops.default}
\alias{dotprops.dotprops}
\alias{dotprops.neuron}
\alias{dotprops.neuronlist}
\alias{is.dotprops}
\title{dotprops: Neurons as point clouds with tangent vectors (but no connectivity)}
\usage{
is.dotprops(x)

as.dotprops(x, ...)

dotprops(x, ...)

\method{dotprops}{dotprops}(x, k = attr(x, "k"), ...)

\method{dotprops}{neuronlist}(x, ...)

\method{dotprops}{neuron}(x, Labels = NULL, ...)

\method{dotprops}{default}(x, k = NULL, Labels = NULL, na.rm = FALSE, ...)
}
\arguments{
  \item{x}{Object to be tested/converted}

  \item{...}{Additional arguments passed to methods}

  \item{k}{Number of nearest neighbours to use for tangent
  vector calculation (set to k=20 when passed NULL)}

  \item{Labels}{Vector of labels for each point or
  \code{NULL} to accept class-specific default behaviour
  for different S3 classes, \code{TRUE} always to use
  labels when incoming object has them and \code{FALSE}
  never to use labels.}

  \item{na.rm}{Whether to remove \code{NA} points (default
  FALSE)}
}
\description{
dotprops: Neurons as point clouds with tangent vectors (but
no connectivity)

\code{dotprops} makes dotprops representation from raw 3d
points (extracting vertices from S3 objects that have them)
}
\details{
\code{k} will default to 20 nearest neighbours when unset
(i.e. when it has default value of NA) unless \code{x} is a
dotprops object.
}
\references{
The dotprops format is essentially identical to that
developed in:

Masse N.Y., Cachero S., Ostrovsky A., and Jefferis G.S.X.E.
(2012). A mutual information approach to automate
identification of neuronal clusters in \emph{Drosophila}
brain images. Frontiers in Neuroinformatics 6 (00021).
\href{http://dx.doi.org/10.3389/fninf.2012.00021}{doi:
10.3389/fninf.2012.00021}
}

