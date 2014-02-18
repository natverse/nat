\name{im3d-io}
\alias{im3d-io}
\alias{read.im3d}
\alias{write.im3d}
\title{Read/Write calibrated 3D blocks of image data}
\usage{
read.im3d(file, ReadData = TRUE, SimplifyAttributes = FALSE, ...)

write.im3d(x, file, ...)
}
\arguments{
  \item{file}{Character vector describing a single file}

  \item{ReadData}{Whether to read the data itself or return
  metadata only.  Default: TRUE}

  \item{SimplifyAttributes}{When \code{TRUE} leave only
  core im3d attributes.}

  \item{...}{Arguments passed to methods}

  \item{x}{The image data to write (an im3d, or capable of
  being interpreted as such)}
}
\value{
For \code{read.im3d} an objecting inheriting from base
\code{array} and \code{im3d} classes.
}
\description{
Read/Write calibrated 3D blocks of image data
}
\details{
Currently only nrrd and amira formats are implemented.
Furthermore implementing a registry to allow extension to
arbitrary formats remains a TODO item.
}
\seealso{
\code{\link{read.nrrd}, \link{read.amiramesh}}

\code{\link{write.nrrd}}
}

