\name{seglengths}
\alias{seglengths}
\title{Calculate length of all segments in neuron}
\usage{
seglengths(x, all = FALSE)
}
\arguments{
  \item{x}{A neuron}

  \item{all}{Whether to calculate lengths for all segments
  when there are multiple subtrees (default: \code{FALSE})}
}
\value{
A vector of lengths for each segment.
}
\description{
Calculate length of all segments in neuron
}
\details{
Only segments in x$SegList will be calculated. Segments
containing only one point will have 0 length.
}
\examples{
summary(seglengths(Cell07PNs[[1]]))
}

