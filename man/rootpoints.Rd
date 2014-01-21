\name{rootpoints}
\alias{branchpoints}
\alias{branchpoints.default}
\alias{branchpoints.igraph}
\alias{branchpoints.neuron}
\alias{endpoints}
\alias{endpoints.default}
\alias{endpoints.igraph}
\alias{endpoints.neuron}
\alias{rootpoints}
\alias{rootpoints.default}
\alias{rootpoints.igraph}
\alias{rootpoints.neuron}
\title{Return the root or branch points of a neuron or graph}
\usage{
rootpoints(x, ...)

\method{rootpoints}{default}(x, ...)

\method{rootpoints}{neuron}(x, subtrees = 1, ...)

\method{rootpoints}{igraph}(x, ...)

branchpoints(x, ...)

\method{branchpoints}{default}(x, ...)

\method{branchpoints}{neuron}(x, subtrees = 1, ...)

\method{branchpoints}{igraph}(x, ...)

endpoints(x, ...)

\method{endpoints}{neuron}(x, subtrees = 1, ...)

\method{endpoints}{igraph}(x, ...)

\method{endpoints}{default}(x, ...)
}
\arguments{
  \item{x}{Neuron or other object which might have roots}

  \item{...}{Further arguments passed to methods}

  \item{subtrees}{Integer index of the fully connected
  subtree in \code{x$SubTrees}. Only applicable when a
  \code{neuron} consists of multiple unconnected subtrees.}
}
\value{
Integer point number of root/branch point
}
\description{
A neuron may have multiple subtrees and therefore multiple
roots

Return the branchpoints of a neuron or graph
}
\details{
\code{branchpoints.neuron} returns a list if more than one
subtree is specified
}

