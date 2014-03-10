\name{remotesync}
\alias{remotesync}
\alias{remotesync.neuronlistfh}
\title{Synchronise a remote object}
\usage{
remotesync(x, remote = attr(x, "remote"), download.missing = TRUE,
  delete.extra = FALSE, ...)

\method{remotesync}{neuronlistfh}(x, remote = attr(x, "remote"),
  download.missing = FALSE, delete.extra = FALSE, update.object = TRUE,
  ...)
}
\arguments{
  \item{x}{Object to synchronise with a remote URL}

  \item{remote}{The remote URL to update from}

  \item{download.missing}{Whether to download missing
  objects (default TRUE)}

  \item{delete.extra}{Whether to delete objects (default
  TRUE)}

  \item{\dots}{Additional arguments passed to methods}

  \item{update.object}{Whether to update the
  \code{neuronlistfh} object itself on disk (default TRUE).
  Note that this assumes that the \code{neuronlistfh}
  object has not been renamed after it was downloaded.}
}
\value{
The updated \code{neuronlistfh} object (invisibly)
}
\description{
Synchronise a remote object
}
\examples{
\dontrun{
kcs20=read.neuronlistfh('kcs20.rds')
kcs20=remotesync(kcs20)
}
}
\seealso{
Other neuronlistfh: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}};
\code{\link{read.neuronlistfh}};
\code{\link{write.neuronlistfh}}
}

