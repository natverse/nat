\name{neuron}
\alias{as.neuron}
\alias{is.neuron}
\alias{neuron}
\title{neuron: class to represent traced neurons}
\usage{
neuron(d, StartPoint, BranchPoints = integer(), EndPoints, SegList,
  SubTrees = NULL, InputFileName = NULL, NeuronName = NULL, ...,
  MD5 = TRUE)

is.neuron(x, Strict = FALSE)

as.neuron(x)
}
\arguments{
  \item{d}{matrix of vertices and associated data in SWC
  format}

  \item{StartPoint,BranchPoints,EndPoints}{Nodes of the
  neuron}

  \item{SegList}{List where each element contains the
  vertex indices for a single segments of the neuron,
  starting at root.}

  \item{SubTrees}{List of SegLists where a neuron has
  multiple unconnected trees (e.g. because the soma is not
  part of the graph, or because the neuronal arbour has
  been cut.)}

  \item{...}{Additional fields to be included in neuron.
  Note that if these include CreatedAt, NodeName,
  InputFileStat or InputFileMD5, they will override fields
  of that name that are calculated automatically.}

  \item{InputFileName}{Character vector with path to input
  file}

  \item{NeuronName}{Character vector containing name of
  neuron}

  \item{MD5}{Logical indicating whether to calculate MD5
  hash of input}

  \item{x}{A neuron or other object to test/convert}

  \item{Strict}{Whether to check class of neuron or use a
  more relaxed definition based on object being a list with
  a SegList component.}
}
\description{
\code{neuron} makes a neuron object from appropriate
variables.

\code{is.neuron} will check if an object looks like a
neuron.

\code{as.neuron} will add class "neuron" to a neuron-like
object.
}
\details{
neuron objects consist of a list containing multiple fields
describing the 3D location and connectivity of points in a
traced neuron. The critical fields of a neuron, n, are n$d
which contains a dataframe in SWC format and n$SegList
which contains a representation of the neuron's topology
used for most internal calculations. Useful functions
include plot.neuron plot3d.neuron write.neuron read.neuron

StartPoint,BranchPoints,EndPoints are indices matching the
rows of the vertices in \code{d} \strong{not} arbitrary
point numbers typically encoded in \code{d$PointNo}.
}
\seealso{
\code{\link{neuronlist}}
}

