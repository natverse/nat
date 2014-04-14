\name{prune}
\alias{prune}
\alias{prune.default}
\alias{prune.dotprops}
\alias{prune.neuron}
\title{prune an object by removing points near (or far) from a target object}
\usage{
prune(x, target, ...)

\method{prune}{neuron}(x, target, ...)

\method{prune}{dotprops}(x, target, ...)

\method{prune}{default}(x, target, maxdist, keep = c("near", "far"),
  return.indices = FALSE, ...)
}
\arguments{
  \item{x}{The object to prune. (e.g. \code{dotprops}
  object, see details)}

  \item{target}{Another object with 3d points that will
  determine which points in x are kept.}

  \item{...}{Additional arguments for methods}

  \item{maxdist}{The threshold distance for keeping points}

  \item{keep}{Whether to keep points in x that are near or
  far from the target}

  \item{return.indices}{Whether to return the indices that
  pass the test rather than the 3d object/points (default
  FALSE)}
}
\description{
prune an object by removing points near (or far) from a
target object
}
\examples{
plot3d(kcs20[[1]],col='blue')
plot3d(kcs20[[2]],col='red')
# prune neuron 2 down to points that are close to neuron 1
neuron2_close=prune(kcs20[[2]], target=kcs20[[1]], maxdist=10)
plot3d(neuron2_close, col='cyan', lwd=3)
neuron2_far=prune(kcs20[[2]], target=kcs20[[1]], maxdist=10, keep='far')
plot3d(neuron2_far, col='magenta', lwd=3)
}
\seealso{
\code{\link{subset.dotprops}}
}

