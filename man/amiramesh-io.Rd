\name{read.amiramesh}
\alias{read.amiramesh}
\alias{read.amiramesh.header}
\title{Read AmiraMesh data in binary or ascii format}
\usage{
read.amiramesh(file, sections = NULL, header = FALSE, simplify = TRUE,
  endian = NULL, Verbose = FALSE)

read.amiramesh.header(file, Verbose = FALSE)
}
\arguments{
  \item{file}{Name of file (or connection) to read}

  \item{sections}{character vector containing names of
  sections}

  \item{header}{Whether to include the full unprocessesd
  text header as an attribute of the returned list.}

  \item{simplify}{If there is only one datablock in file do
  not return wrapped in a list (default TRUE).}

  \item{endian}{Whether multibyte data types should be
  treated as big or little endian. Default of NULL checks
  file or uses \code{.Platform$endian}}

  \item{Verbose}{Print status messages}
}
\value{
list of named data chunks
}
\description{
Read AmiraMesh data in binary or ascii format

Read the header of an amiramesh file
}
\details{
\code{read.amiramesh.header} will open a connection if file
is a character vector and close it when finished reading.
}
\seealso{
\code{\link{readBin}, \link{.Platform}}

Other amira: \code{\link{amiratype}};
\code{\link{is.amiramesh}};
\code{\link{read.amiralandmarks}},
\code{\link{write.amiralandmarks}};
\code{\link{read.hxsurf}}
}

