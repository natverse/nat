\name{read.neuron}
\alias{read.neuron}
\title{Read a single neuron from a file}
\usage{
read.neuron(f, format = NULL, ...)
}
\arguments{
  \item{f}{Path to file}

  \item{format}{The file format of the neuron. When
  \code{format=NULL}, the default, \code{read.neuron} will
  infer the file format from the extension or file header
  (aka magic) using the \code{fileformats} registry.}

  \item{...}{additional arguments passed to format-specific
  readers}
}
\description{
Read a single neuron from a file
}
\details{
This function will handle \code{neuron} and \code{dotprops}
objects saved in R .rds or .rda format by default.
Additional file formats can be registered using
\code{fileformats}.
}
\examples{
\dontrun{
# note that we override the default NeuronName field
n=read.neuron(system.file("tests/testthat/testdata","neuron","EBT7R.CNG.swc",package='nat'),
  NeuronName="EBT7R")
# use a function to set the NeuronName field
n3=read.neuron(system.file("tests/testthat/testdata","neuron","EBT7R.CNG.swc",package='nat'),
  NeuronName=function(x) sub("\\\\..*","",x))
# show the currently registered file formats that we can read
fileformats(class='neuron', read=TRUE)
}
}
\seealso{
\code{\link{read.neurons}, \link{fileformats}}
}

