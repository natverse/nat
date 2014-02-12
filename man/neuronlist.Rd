\name{is.neuronlist}
\alias{is.neuronlist}
\alias{neuronlist}
\title{Create and test objects of neuronlist class to store multiple neurons}
\usage{
is.neuronlist(x)

neuronlist(..., DATAFRAME = NULL)
}
\arguments{
  \item{x}{A neuronlist object}

  \item{...}{objects to be turned into a list}

  \item{DATAFRAME}{an optional data.frame to attach to the
  neuronlist containing information about each neuron.}
}
\value{
return value

return value
}
\description{
\code{neuronlist} objects consist of a list of neuron
objects along with an optional attached dataframe
containing information about the neurons. \code{neuronlist}
objects can be indexed using their name or the number of
the neuron like a regular list. If the \code{[} operator is
used to index the list, the attached dataframe will also be
subsetted.

It is perfectly acceptable not to pass any parameters,
generating an empty neuronlist
}
\details{
\code{is.neuronlist} Uses a relaxed definition to cope with
older lists of neurons that do not have a class attribute
of neuronlist
}
\examples{
# generate an empty neuronlist
nl=neuronlist()
# slice an existing neuronlist with regular indexing
kcs5=kcs20[1:5]
}
\seealso{
Other neuronlist: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}}; \code{\link{nlapply}};
\code{\link{read.neurons}}; \code{\link{write.neurons}}
}

