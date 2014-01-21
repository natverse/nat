\name{read.neuron}
\alias{read.neuron}
\title{Read a single neuron from a file}
\usage{
read.neuron(f, ...)
}
\arguments{
  \item{f}{Path to file}

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
\code{neuronformats}.
}
\examples{
# note that we override the default NeuronName field
n=read.neuron(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'),
  NeuronName="EBT7R")
# use a function to set the NeuronName field
n3=read.neuron(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'),
  NeuronName=function(x) sub("\\\\..*","",x))
}
\seealso{
\code{\link{read.neurons}, \link{neuronformats}}
}

