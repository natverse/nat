\name{plot.neuronlist}
\alias{plot.neuronlist}
\title{2D plots of the elements in a neuronlist, optionally using a subset
expression}
\usage{
\method{plot}{neuronlist}(x, subset, LineCol = NULL, colpal = rainbow, ...)
}
\arguments{
  \item{x}{a neuron list or, for \code{plot3d.character}, a
  character vector of neuron names. The default neuronlist
  used by plot3d.character can be set by using
  \code{options(nat.default.neuronlist='mylist')}. See
  ?\code{\link{nat}} for details.
  \code{\link{nat-package}}.}

  \item{LineCol}{An expression specifying a colour
  evaluated in the context of the dataframe attached to nl
  (after any subsetting). See details.}

  \item{...}{options passed on to plot (such as colours,
  line width etc)}

  \item{subset}{Expression evaluating to logical mask for
  neurons. See details.}

  \item{colpal}{A vector of colours or a function that
  generates colours}
}
\value{
list of values of \code{plot} with subsetted dataframe as
attribute \code{'df'}
}
\description{
2D plots of the elements in a neuronlist, optionally using
a subset expression
}
\details{
The col and subset parameters are evaluated in the context
of the dataframe attribute of the neuronlist. If col
evaluates to a factor and colpal is a named vector then
colours will be assigned by matching factor levels against
the named elements of colpal. If col evaluates to a factor
and colpal is a function then it will be used to generate
colours with the same number of levels as are used in col.
}
\seealso{
\code{\link{nat-package}, \link{plot3d.neuronlist}}
}

