\name{potential_synapses}
\alias{potential_synapses}
\alias{potential_synapses.neuron}
\alias{potential_synapses.neuronlist}
\title{Calculate number of potential synapses between two neurons}
\usage{
potential_synapses(a, b, s, ...)

\method{potential_synapses}{neuronlist}(a, b, s, ...)

\method{potential_synapses}{neuron}(a, b, s, sigma = s, bounds,
  method = c("direct", "approx"), ...)
}
\arguments{
  \item{a,b}{neurons or neuronlists}

  \item{s}{the approach distance to consider a potential
  synapse}

  \item{sigma}{the smoothing parameter in the approximate
  method (see details)}

  \item{method}{Whether to use the direct or approximate
  method (see details)}

  \item{bounds}{Optional bounding box to restrict
  comparison}

  \item{...}{Additional arguments passed to methods}
}
\description{
This implements the method of Stepanyants and Chklovskii
}
\examples{
potential_synapses(Cell07PNs[1], Cell07PNs[1:3], s=2)
}
\references{
Neurogeometry and potential synaptic connectivity.
Stepanyants A, Chklovskii DB. Trends Neurosci. 2005
Jul;28(7):387-94.
}

