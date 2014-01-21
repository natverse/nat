\name{read.neurons}
\alias{read.neurons}
\title{Read one or more neurons from file to a neuronlist in memory}
\usage{
read.neurons(paths, pattern = NULL, neuronnames = basename, nl = NULL,
  df = NULL, OmitFailures = TRUE, SortOnUpdate = FALSE, ...)
}
\arguments{
  \item{paths}{Paths to neuron input files (or directory
  containing neurons)}

  \item{pattern}{If paths is a directory, regex that file
  names must match.}

  \item{neuronnames}{Character vector or function that
  specifies neuron names}

  \item{df}{Optional data frame containing information
  about each neuron}

  \item{OmitFailures}{Omit failures (when TRUE) or leave an
  NA value in the list}

  \item{SortOnUpdate}{Sort the neuronlist when update adds
  new neurons}

  \item{nl}{An existing neuronlist to be updated (see
  details)}

  \item{...}{Additional arguements to passed to read.neuron
  methods}
}
\value{
neuronlist object containing the neurons
}
\description{
Read one or more neurons from file to a neuronlist in
memory
}
\details{
This function will cope with the same set of file formats
offered by \code{read.neuron}. If the \code{paths} argument
specifies a (single) directory then all files in that
directory will be read unless an optional regex pattern is
also specified. \code{neuronnames} must specify a unique
set of names that will be used as the names of the neurons
in the resultant neuronlist. If \code{neuronnames} is a a
function then this will be applied to the path to each
neuron. The default value is the function \code{basename}
which results in each neuron being named for the input file
from which it was read. The optional dataframe (\code{df})
detailing each neuron should have \code{rownames} that
match the names of each neuron. It would also make sense if
the same key was present in a column of the data frame. If
the dataframe contains more rows than neurons, the
superfluous rows are dropped with a warning. If the
dataframe is missing rows for some neurons an error is
generated. If SortOnUpdate is TRUE then updating an
existing neuronlist should result in a new neuronlist with
ordering identical to reading all neurons from scratch.
}
\seealso{
\code{\link{read.neuron}}
}

