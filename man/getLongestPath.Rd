\name{getLongestPath}
\alias{getLongestPath}
\title{The longest path along a neuron}
\usage{
getLongestPath(n, SpatialWeights = TRUE, ReturnPath = FALSE)
}
\arguments{
  \item{n}{the neuron to consider.}

  \item{SpatialWeights}{logical indicating whether spatial
  distances (default) should be used to weight segments
  instead of weighting each equally.}

  \item{ReturnPath}{logical indicating whether the length
  of the path (default) or the sequence of vertices along
  the path should be returned.}
}
\value{
Either the length of the longest path along the neuron (if
\code{ReturnPath == FALSE}) or a list of vertices.
}
\description{
The longest path along a neuron
}

