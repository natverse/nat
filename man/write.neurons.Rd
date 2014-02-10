\name{write.neurons}
\alias{write.neurons}
\title{Write neurons from a neuronlist object to individual files}
\usage{
write.neurons(nl, dir, subdir = NULL, INDICES = names(nl), ...)
}
\arguments{
  \item{nl}{neuronlist object}

  \item{dir}{directory to write neurons}

  \item{subdir}{String naming field in neuron that
  specifies a subdirectory OR expression to evaluate in the
  context of neuronlist's df attribute}

  \item{INDICES}{Character vector of the names of a subset
  of neurons in neuronlist to write.}

  \item{...}{Additional arguments passed to write.neuron}
}
\description{
Write neurons from a neuronlist object to individual files
}
\examples{
\dontrun{
write.neurons(Cell07PNs,dir="testwn",
  subdir=file.path(Glomerulus,Scored.By),format='hxlineset')
# only write a subset
write.neurons(subset(Cell07PNs, Scored.By="ACH"),dir="testwn2",
  subdir=file.path(Glomerulus),format='hxlineset')
# The same, but likely faster for big neuronlists
write.neurons(Cell07PNs, dir="testwn3",
  INDICES=subset(Cell07PNs,Scored.By="ACH",rval='names'),
  subdir=file.path(Glomerulus),format='hxlineset')
}
}
\author{
jefferis
}
\seealso{
\code{\link{write.neuron}}

Other neuronlist: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}}; \code{\link{is.neuronlist}},
\code{\link{neuronlist}}; \code{\link{nlapply}};
\code{\link{read.neurons}}
}

