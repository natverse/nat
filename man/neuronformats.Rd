\name{neuronformats}
\alias{getformatreader}
\alias{getformatwriter}
\alias{neuronformats}
\alias{registerformat}
\title{Set or return list of registered file formats that we can read}
\usage{
neuronformats(format = NULL, ext = NULL, read = NULL, write = NULL,
  class = NULL, rval = c("names", "info", "all"))

registerformat(format = NULL, ext = format, read = NULL, write = NULL,
  magic = NULL, magiclen = NA_integer_, class = NULL)

getformatreader(file, class = NULL)

getformatwriter(format = NULL, file = NULL, ext = NULL, class = NULL)
}
\arguments{
  \item{format}{Character vector naming the format}

  \item{ext}{Character vector of file extensions}

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
\description{
\code{neuronformats} returns format names, functions or a
table of info re functions that match a filter.

\code{registerformat} registers a format in the io registry

\code{getformatwriter} gets the function to read a file

\code{getformatwriter} gets the function to write a file
}
\details{
\code{getformatreader} starts by reading a set number of
bytes from the start off the current file and then checks
using file extension and magic functions to see if it can
identify the file. Presently formats are in a queue in
alphabetical order, dispatching on the first match.
}

