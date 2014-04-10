\name{spine}
\alias{spine}
\title{The longest path along a neuron}
\usage{
spine(n, SpatialWeights = TRUE, ReturnPath = FALSE, PlotPath = FALSE)
}
\arguments{
  \item{n}{the neuron to consider.}

  \item{SpatialWeights}{logical indicating whether spatial
  distances (default) should be used to weight segments
  instead of weighting each equally.}

  \item{ReturnPath}{logical indicating whether the length
  of the path (default) or the sequence of vertices along
  the path should be returned.}

  \item{PlotPath}{logical indicating whether the path
  should be plotted in 3D (default == TRUE).}
}
\value{
Either the length of the longest path along the neuron (if
\code{ReturnPath == FALSE}) or a neuron object
corresponding to the longest path.
}
\description{
The longest path along a neuron
}

