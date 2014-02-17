\name{read.im3d}
\alias{read.im3d}
\title{Read a 3D block of image data}
\usage{
read.im3d(file, ...)
}
\arguments{
  \item{file}{Character vector describing a single file to
  read}

  \item{...}{Arguments passed to methods}
}
\description{
Read a 3D block of image data
}
\details{
Currently only nrrd and amira formats are implemented.
Furthermore implementing a registry to allow extension to
arbitrary formats remains a TODO item.
}
\seealso{
\code{\link{read.nrrd}, \link{read.amiramesh}}
}

