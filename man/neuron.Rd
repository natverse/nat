\name{is.neuron}
\alias{as.neuron}
\alias{is.neuron}
\title{neuron: class to represent traced neurons}
\usage{
  is.neuron(n, Strict = FALSE)

  as.neuron(n)
}
\arguments{
  \item{n}{A neuron}

  \item{Strict}{Whether to check class of neuron or use a
  more relaxed definition based on object being a list with
  a SegList component.}
}
\description{
  \code{is.neuron} will check if an object looks like a
  neuron.

  \code{as.neuron} will add class "neuron" to a neuron-like
  object.
}
\details{
  neuron objects consist of a list containing multiple
  fields describing the 3D location and connectivity of
  points in a traced neuron. The critical fields of a
  neuron, n, are n$d which contains a dataframe in SWC
  format and n$SegList which contains a representation of
  the neuron's topology used for most internal
  calculations. Useful functions include plot.neuron
  plot3d.neuron write.neuron read.neuron
}
\seealso{
  neuronlist
}

