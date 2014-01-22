\name{as.neuronlist}
\alias{as.neuronlist}
\title{Make a list of neurons that can be used for coordinate plotting/analysis}
\usage{
as.neuronlist(l, ...)
}
\arguments{
  \item{l}{An existing list or a single neuron to start a
  list}

  \item{...}{Additional arguments passed to methods}
}
\value{
neuronlist with attr('df')
}
\description{
Note that it can cope with both neurons and dotprops but
AddClassToNeurons parameter will only apply to things that
look like neurons but don't have a class of neuron.
}
\seealso{
\code{\link{is.neuronlist}},\code{\link{is.neuron}},\code{\link{is.dotprops}}
}

