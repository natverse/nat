\name{as.neuronlist}
\alias{as.neuronlist}
\title{Make a list of neurons that can be used for coordinate plotting/analysis}
\usage{
  as.neuronlist(l, df, AddClassToNeurons = TRUE)
}
\arguments{
  \item{l}{An existing list or a single neuron to start a
  list}

  \item{df}{A dataframe with one row of information per
  neuron}

  \item{AddClassToNeurons}{make sure that list elements
  have class neuron.}
}
\value{
  neuronlist with attr('df')
}
\description{
  Note that it can cope with both neurons and dotprops but
  AddClassToNeurons parameter will only apply to things
  that look like neurons but don't have a class of neuron.
}
\seealso{
  \code{\link{is.neuronlist}},\code{\link{is.neuron}},\code{\link{is.dotprops}}
}

