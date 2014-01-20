\name{ngraph}
\alias{as.ngraph}
\alias{as.ngraph.data.frame}
\alias{as.ngraph.neuron}
\alias{ngraph}
\title{ngraph: a graph to encode a neuron's connectivity}
\usage{
ngraph(el, vertexlabels, xyz = NULL, directed = TRUE,
  graph.attributes = NULL)

as.ngraph(x, ...)

\method{as.ngraph}{data.frame}(x, directed = TRUE, ...)

\method{as.ngraph}{neuron}(x, directed = TRUE, method = c("swc", "seglist"),
  ...)
}
\arguments{
  \item{el}{A two columm matrix (start, end) defining
  edges}

  \item{vertexlabels}{Integer labels for graph - the edge
  list is specified using these labels.}

  \item{xyz}{3D coordinates of vertices (optional, Nx3
  matrix)}

  \item{directed}{Whether the resultant graph should be
  directed (default TRUE)}

  \item{graph.attributes}{List of named attributes to be
  added to the graph}

  \item{x}{Object to convert (see method descriptions)}

  \item{...}{Arguments passed to methods}

  \item{method}{Whether to use the swc data (x$d) or the
  seglist to define neuronal connectivity to generate
  graph.}
}
\value{
an \code{igraph} object with additional class
\code{ngraph}, having a vertex for each entry in
vertexlabels, each vertex having a \code{label} attribute.
All vertices are included whether connected or not.
}
\description{
ngraph: a graph to encode a neuron's connectivity

\code{as.ngraph} converts an object to an ngraph

\code{as.ngraph.dataframe} construct ngraph from a
data.frame containing SWC format data

\code{as.ngraph.neuron} construct ngraph from a neuron
}
\details{
We make the following assumptions about neurons coming in
\itemize{ \item They have an integer vertex label that need
not start from 1 and that may have gaps \item The edge list
which defines connectivity specifies edges using pairs of
vertex labels, _not_ raw vertex ids. }

Note that this method \emph{always} keeps the original
vertex labels (a.k.a. PointNo) as read in from the original
file.
}
\seealso{
\code{\link{igraph}}

Other neuron: \code{\link{as.neuron}},
\code{\link{as.neuron.data.frame}},
\code{\link{as.neuron.default}},
\code{\link{as.neuron.ngraph}}, \code{\link{is.neuron}},
\code{\link{neuron}}
}

