\name{read.amiralandmarks}
\alias{read.amiralandmarks}
\alias{write.amiralandmarks}
\title{Read and Write Amira Landmarks files}
\usage{
read.amiralandmarks(file, CoordinatesOnly = TRUE, Verbose = FALSE)

write.amiralandmarks(x, file)
}
\arguments{
  \item{file}{The file to read or write}

  \item{CoordinatesOnly}{Only return Coordinates of points}

  \item{Verbose}{Whether to write status messages}

  \item{x}{Nx3 matrix or dataframe of landmark coordinates
  or a list with two matrices, one for landmarks in each
  space.}
}
\value{
for read.amiralandmarks a matrix for an unpaired landmark
set or a list of length 2 for a paired landmark set.
}
\description{
Read and Write Amira Landmarks files
}
\seealso{
Other amira: \code{\link{amiratype}};
\code{\link{is.amiramesh}}; \code{\link{read.amiramesh}},
\code{\link{read.amiramesh.header}};
\code{\link{read.hxsurf}}
}

