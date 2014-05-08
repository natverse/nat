\name{seglengths}
\alias{seglengths}
\title{Calculate length of all segments in neuron}
\usage{
seglengths(x)
}
\arguments{
  \item{x}{A neuron}
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
\seealso{
\code{\link{resample}}
}

