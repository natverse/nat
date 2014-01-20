\name{rootpoints}
\alias{branchpoints}
\alias{branchpoints.default}
\alias{branchpoints.igraph}
\alias{branchpoints.neuron}
\alias{endpoints}
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

\method{rootpoints}{default}(x, ...)

\method{branchpoints}{neuron}(x, subtrees = 1, ...)

\method{branchpoints}{igraph}(x, ...)

endpoints(x, ...)

\method{endpoints}{neuron}(x, subtrees = 1, ...)

\method{endpoints}{igraph}(x, ...)
}
\arguments{
  \item{x}{Neuron or other object which might have roots}

  \item{...}{}

  \item{x}{neuron or graph}

  \item{...}{Additional parameters}
}
\value{
Integer point number of root/branch point
}
\description{
A neuron may have multiple subtrees and therefore multiple
roots

Return the branchpoints of a neuron or graph
}

