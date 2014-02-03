\name{read.neuronlistfh}
\alias{read.neuronlistfh}
\title{Read a local, or remote, neuronlistfh object saved to a file.}
\usage{
read.neuronlistfh(file, localdir = NULL, ...)
}
\arguments{
  \item{file}{The file path of the neuronlistfh object. Can
  be local, or remote (via http or ftp).}

  \item{localdir}{If the file is to be fetched from a
  remote location, this is the folder in which downloaded
  objects will be stored.}

  \item{...}{Extra arguments to pass to
  \code{download.file}.}
}
\description{
Read a local, or remote, neuronlistfh object saved to a
file.
}
\details{
When reading a remote \code{neuronlistfh} object, it is
downloaded and cached to \code{localdir}. If there is
already a cached file at the appropriate location then the
md5sums are checked and the downloaded file will be copied
on top of the original copy if they are different.
}

