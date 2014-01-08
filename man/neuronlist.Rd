\name{is.neuronlist}
\alias{is.neuronlist}
\alias{neuronlist}
\title{Create and test objects of neuronlist class to store multiple neurons}
\usage{
  is.neuronlist(x)

  neuronlist(...)
}
\arguments{
  \item{x}{A neuronlist object}

  \item{...}{objects to be turned into a list}
}
\value{
  return value

  return value
}
\description{
  \code{neuronlist} objects consist of a list of neuron
  objects along with an optional attached dataframe
  containing information about the neurons. Relevant
  functions include plot3d.neuronlist write.neuronlist
  subset.neuronlist read.neurons can generate a neuronlist

  It is perfectly acceptable not to pass any parameters,
  generating an empty neuronlist
}
\details{
  \code{is.neuronlist} Uses a relaxed definition to cope
  with older lists of neurons that do not have a class
  attribute of neuronlist
}
\examples{
# generate an empty neuronlist
nl=neuronlist()
}

