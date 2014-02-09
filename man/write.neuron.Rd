\name{write.neuron}
\alias{write.neuron}
\title{Write out a neuron in any of the file formats we know about}
\usage{
write.neuron(n, file = NULL, dir = NULL, format = NULL, ext = NULL, ...)
}
\arguments{
  \item{n}{A neuron}

  \item{file}{Path to output file}

  \item{dir}{Path to directory (this will replace
  dirname(file) if specified)}

  \item{format}{Unique abbreviation of one of the
  registered file formats for neurons including 'swc',
  'hxlineset', 'hxskel'}

  \item{ext}{Will replace the default extension for the
  filetype and should include the period eg
  \code{ext='.amiramesh'} or \code{ext='_reg.swc'}}

  \item{...}{Additional arguments passed to selected writer
  function}
}
\value{
return value
}
\description{
If file is not specified the neuron's Inputfile field will
be checked. If this is missing there will be an error. If
dir is specified it will be combined with basename(file).
If file is specified but format is not, it will be inferred
from file's extension.
}
\seealso{
\code{\link{fileformats}, \link{saveRDS}}
}

