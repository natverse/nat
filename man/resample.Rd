\name{resample}
\alias{resample}
\alias{resample.neuron}
\title{Resample an object with a new spacing}
\usage{
resample(x, ...)

\method{resample}{neuron}(x, stepsize, ...)
}
\arguments{
  \item{x}{An object to resample}

  \item{...}{Additional arguments passed to methods}

  \item{stepsize}{The new spacing along the tracing}
}
\description{
Resample an object with a new spacing

resample a neuron with a new spacing
}
\details{
\code{resample.neuron} calls seglengths to calculate the
length of each segment of the neuron before resampling.
}
\seealso{
\code{\link{seglengths}}
}

