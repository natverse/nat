\name{drawLongestPath}
\alias{drawLongestPath}
\title{Draw the longest path along a neuron}
\usage{
drawLongestPath(n, SpatialWeights = TRUE, lw = 4, ...)
}
\arguments{
  \item{n}{the neuron to consider.}

  \item{SpatialWeights}{logical indicating whether spatial
  distances (default) should be used to weight segments
  instead of weighting each equally.}

  \item{lw}{the width of the line used to draw the path.}

  \item{...}{extra arguments passed to
  \code{\link[rgl]{lines3d}}.}
}
\value{
A list of plotted objects (invisibly).
}
\description{
Draw the longest path along a neuron
}
\seealso{
\code{\link[rgl]{lines3d}}
}

