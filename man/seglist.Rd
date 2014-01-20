\name{seglist}
\alias{as.seglist}
\alias{as.seglist.igraph}
\alias{seglist}
\title{Make/convert neuron connectivity information into a seglist object}
\usage{
seglist(x = NULL)

as.seglist(x, ...)

\method{as.seglist}{igraph}(x, origin = NULL, Verbose = FALSE, ...)
}
\arguments{
  \item{x}{Object containing connectivity information}

  \item{...}{Arguments passed to methods}

  \item{origin}{The origin of the tree (see details)}

  \item{Verbose}{Whether to print progress updates to
  console (default FALSE)}
}
\value{
a \code{list} with one entry for each unbranched segment.
}
\description{
\code{seglist} makes a seglist object from a list of
integer vectors of raw vertex ids

\code{as.seglist.igraph} will convert a fully connected
acyclic ngraph or igraph object into a seglist consisting
of exactly one subtree.
}
\details{
see \code{\link{neuron}} for further information about
seglists.

If the graph vertices have \code{vid} attributes, typically
defining the original vertex ids of a graph that was then
decomposed into subgraphs, then the origin is assumed to
refer to one of these vids not a raw vertex id of the
current graph. The returned seglist will also contain these
original vertex ids.
}
\seealso{
\code{\link{neuron}}

\code{\link{ngraph},\link{igraph}}
}

