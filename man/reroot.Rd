% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neuron.R
\name{reroot}
\alias{reroot}
\alias{reroot.neuron}
\alias{reroot.neuronlist}
\title{Reroot neurons}
\usage{
reroot(x, ...)

\method{reroot}{neuron}(x, idx = NULL, pointno = NULL, point = NULL, ...)

\method{reroot}{neuronlist}(x, idx = NULL, pointno = NULL, point = NULL, ...)
}
\arguments{
\item{x}{A \code{\link{neuron}} or \code{\link{neuronlist}} object}

\item{...}{Additional arguments passed to methods}

\item{idx}{index of the node for the new root (between 1 and the number of
nodes in the neuron).}

\item{pointno}{new root node identifier (i.e. the \code{PointNo} column in
the point array of the neuron, see details).}

\item{point}{3-vector with X,Y,Z coordinates (data.frame or Nx3 matrix for
neuronlist)}
}
\value{
neuron with a new root position (unless \code{idx}, \code{pointno},
  and \code{point} are all \code{NULL}, when the original neuron is
  returned).
}
\description{
Change the root node of a neuron (typically denoting the soma) to a new node
specified by a node index, identifier or an XYZ position.
}
\details{
All neurons in the natverse have a root point, which is used for
  during many operations on the branching structure of the neuron. This will
  often correspond to the soma of a neuron, but the soma is not always
  present and sometimes its position may be unknown. For example some
  connectomics datasets will have a certain position on a neuron marked as
  \code{to soma} when the soma is not present in the reconstruction but it is
  known to which branch it is attached.

  The root point of a neuron is stored in the \code{StartPoint} field of the
  neuron (see Examples) and can also be accessed using the
  \code{\link{rootpoints}} function. For further details, please consult the
  \href{http://natverse.org/nat/articles/neurons-as-graph.html}{Neurons as
  graph structures} vignette. As an extension to the original nat
  specification, the point identifier (not point index) of the anatomical
  soma can be stored in the \code{tags$soma} field of the neuron

  The node index refers is a number between 1 and N, the number of points in
  the neuron. It provides an index into the point array. The node id is an
  arbitrary identifier which may sometime be the same as the index, but may
  be e.g. a 64 bit integer that uniquely identifies nodes across all neurons
  in a database. Node ids can be retained after neurons are pruned even if
  the indices for each point change. For further details, again see the
  vignette mentioned above.
}
\examples{
newCell07PN <- reroot(Cell07PNs[[2]], 5)
newCell07PN$StartPoint # 5
}
