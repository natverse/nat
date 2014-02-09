\name{fileformats}
\alias{fileformats}
\alias{getformatreader}
\alias{getformatwriter}
\alias{registerformat}
\title{Set or return list of registered file formats that we can read}
\usage{
fileformats(format = NULL, ext = NULL, read = NULL, write = NULL,
  class = NULL, rval = c("names", "info", "all"))

registerformat(format = NULL, ext = format, read = NULL, write = NULL,
  magic = NULL, magiclen = NA_integer_, class = NULL)

getformatreader(file, class = NULL)

getformatwriter(format = NULL, file = NULL, ext = NULL, class = NULL)
}
\arguments{
  \item{rval}{Character vector choosing what kind of return
  value \code{fileformats} will give.}

  \item{format}{Character vector naming the format}

  \item{ext}{Character vector of file extensions (including
  periods)}

  \item{read,write}{Functions to read and write this
  format}

  \item{magic}{Function to test whether a file is of this
  format}

  \item{magiclen}{Optional integer specifying maximum
  number of bytes required from file header to determine
  file's type.}

  \item{class}{The S3 class for the format (character
  vector e.g. 'neuron')}

  \item{file}{Path to a file}
}
\value{
\itemize{

\item \code{fileformats} returns a character vector, matrix
or list according to the value of rval.

\item \code{getformatreader} returns a list. The reader can
be accessed with \code{$read}

\item \code{getformatwriter} returns a list. The writer can
be accessed with \code{$write}.}
}
\description{
\code{fileformats} returns format names, a format
definition list or a table of information about the formats
that match the given filter conditions.

\code{registerformat} registers a format in the io registry

\code{getformatwriter} gets the function to read a file

\code{getformatwriter} gets the function to write a file
}
\details{
if a \code{format} argument is passed to \code{fileformats}
it will be matched wigth partial string matching and iif a
unique match exists that will be returned.

\code{getformatreader} starts by reading a set number of
bytes from the start off the current file and then checks
using file extension and magic functions to see if it can
identify the file. Presently formats are in a queue in
alphabetical order, dispatching on the first match.

If \code{ext=NA} then extension will not be used to query
file formats and it will be overwritten by the default
extensions returned by \code{fileformats}. If
\code{ext='.someext'} \code{getformatwriter} will use the
specified extension to overwrite the value returned by
\code{fileformats}. If \code{ext=NULL} and
\code{file='somefilename.someext'} then \code{ext} will be
set to \code{'someext'} and that will override the value
returned by \code{fileformats}. See
\code{\link{write.neuron}} for code to make this discussion
more concrete.
}
\examples{
# information about the currently registered file formats
fileformats(rval='info')
\dontrun{
registerformat("swc",read=read.swc,write=read.swc,magic=is.swc,magiclen=10,
  class='neuron')
}
}

