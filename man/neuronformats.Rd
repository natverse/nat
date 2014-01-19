\name{neuronformats}
\alias{getformatfuns}
\alias{neuronformats}
\title{Set or return list of registered file formats that we can read}
\usage{
neuronformats(format, ext = format, read = NULL, write = NULL,
  magic = NULL, magiclen = NA_integer_, class = NULL)

getformatfuns(f, action = c("read", "write"), class = NULL)
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

  \item{f}{Path to a file}

  \item{action}{Whether we must have a read or write
  function for this file}
}
\description{
Set or return list of registered file formats that we can
read

Get the list of functions (read, write etc) for a file
}

