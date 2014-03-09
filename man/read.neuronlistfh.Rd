\name{read.neuronlistfh}
\alias{read.neuronlistfh}
\title{Read a local, or remote, neuronlistfh object saved to a file.}
\usage{
read.neuronlistfh(file, localdir = NULL, update = FALSE, ...)
}
\arguments{
  \item{file}{The file path of the neuronlistfh object. Can
  be local, or remote (via http or ftp).}

  \item{localdir}{If the file is to be fetched from a
  remote location, this is the folder in which downloaded
  RDS file will be saved. See details.}

  \item{update}{Whether to update local copy of
  neuronlistfh (default: FALSE, see details)}

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
already a cached file at the appropriate location and
\code{update=TRUE} then the md5sums are checked and the
downloaded file will be copied on top of the original copy
if they are different; if \code{udpate=FALSE}, the default,
then no action will be taken.

Note also that there is a strict convention for the layout
of the files on disk. The neuronlistfh object will be saved
in R's \code{RDS} format and will be placed next to a
folder called \code{data} which will contain the data
objects, also saved in RDS format. For example if
\code{myneurons.rds} is downloaded to
\code{localdir="\\path\\to\\localdir"} the resultant file
layout will be as follows:

\itemize{

\item \code{\\path\\to\\localdir\\myneurons.rds}

\item
\code{\\path\\to\\localdir\\data\\2f88e16c4f21bfcb290b2a8288c05bd0}

\item
\code{\\path\\to\\localdir\\data\\5b58e040ee35f3bcc6023fb7836c842e}

\item \code{\\path\\to\\localdir\\data\... etc}

}

Given this arrangment, the data directory should always be
at a fixed location with respect to the saved neuronlistfh
object and this is checked on read and write. If the
neuronlistfh object specified by file does not have a
filehash database with a valid \code{dir} slot and there is
no 'data' directory adjacent to the neuronlistfh object, an
error will result.
}
\seealso{
Other neuronlistfh: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}};
\code{\link{write.neuronlistfh}}
}

