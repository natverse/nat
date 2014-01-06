\name{read.cmtk}
\alias{read.cmtk}
\title{Read CMTK TypedStream file to a list in memory}
\usage{
  read.cmtk(con, CheckLabel = TRUE)
}
\arguments{
  \item{con}{Path to (optionally gzipped) file or (open)
  connection.}

  \item{CheckLabel}{Check, fix and warn for invalid or
  duplicate labels (default TRUE)}
}
\description{
  This function is primarily of developer interest. End
  users will typically want to use more specialised
  functions for reading registrations and landmarks.
}
\details{
  This is the default format used by CMTK for registration,
  studylist, landmarks and image files. Although this is
  largely a generic function, there is special handling of
  the coefficients and active members of the spline warp
  component of a CMTK nonrigid registrartion.
}
\seealso{
  Other cmtk-io: \code{\link{read.cmtkreg}},
  \code{\link{write.cmtk}}, \code{\link{write.cmtkreg}}
}
