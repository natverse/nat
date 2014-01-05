\name{read.cmtkreg}
\alias{read.cmtkreg}
\title{Read a CMTK format registration}
\usage{
  read.cmtkreg(filename, ReturnRegistrationOnly = FALSE,
    ...)
}
\arguments{
  \item{filename}{Path to a CMTK registration file}

  \item{ReturnRegistrationOnly}{When FALSE (default) will
  not attempt to extract the registration element from the
  registration file.}

  \item{...}{Additional arguments passed to read.cmtk}
}
\description{
  Read a CMTK format registration
}
\seealso{
  Other cmtk-io: \code{\link{read.cmtk}},
  \code{\link{write.cmtk}}, \code{\link{write.cmtkreg}}
}

