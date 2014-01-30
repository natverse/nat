\name{amiratype}
\alias{amiratype}
\title{Return the type of an amiramesh file on disk or a parsed header}
\usage{
amiratype(x)
}
\arguments{
  \item{x}{Path to files on disk or a single pre-parsed
  parameter list}
}
\value{
character vector (NA_character_ when file invalid)
}
\description{
Return the type of an amiramesh file on disk or a parsed
header
}
\details{
Note that when checking a file we first test if it is an
amiramesh file (fast) before reading the header and
determining content type (slow).
}
\seealso{
Other amira: \code{\link{is.amiramesh}};
\code{\link{read.amiralandmarks}},
\code{\link{write.amiralandmarks}};
\code{\link{read.amiramesh}},
\code{\link{read.amiramesh.header}};
\code{\link{read.hxsurf}}
}

