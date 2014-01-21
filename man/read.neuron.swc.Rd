\name{read.neuron.swc}
\alias{read.neuron.swc}
\title{read a neuron in swc file format}
\usage{
read.neuron.swc(f, ...)
}
\arguments{
  \item{f}{path to file}

  \item{...}{Additional arguments passed to
  \code{as.neuron()} and then on to \code{neuron()}}
}
\description{
read a neuron in swc file format
}
\section{SWC Format}{
  According to
  \url{http://www.soton.ac.uk/~dales/morpho/morpho_doc} SWC
  file format has a radius not a diameter specification
}
\examples{
n=read.neuron.swc(system.file("testdata","neuron","EBT7R.CNG.swc",package='nat'))
}
\seealso{
\code{\link{read.neuron},\link{neuron}, \link{as.neuron}}
}

