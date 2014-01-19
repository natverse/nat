\name{ngraph}
\alias{as.ngraph}
\alias{as.ngraph.data.frame}
\alias{ngraph}
\title{ngraph: a graph to encode a neuron's connectivity}
\usage{
ngraph(el, vertexlabels, xyz = NULL, directed = TRUE,
  graph.attributes = NULL)

as.ngraph(x, ...)

\method{as.ngraph}{data.frame}(x, directed = TRUE, ...)
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
}
\details{
We make the following assumptions about neurons coming in
\itemize{ \item They have an integer vertex label that need
not start from 1 and that may have gaps \item The edge list
which defines connectivity specifies edges using pairs of
vertex labels, _not_ raw vertex ids. }
}

