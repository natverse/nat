\name{xyzmatrix}
\alias{xyzmatrix}
\alias{xyzmatrix.default}
\alias{xyzmatrix.igraph}
\alias{xyzmatrix<-}
\title{Get and assign coordinates for classes containing 3d vertex data}
\usage{
xyzmatrix(x, ...)

\method{xyzmatrix}{default}(x, y = NULL, z = NULL, ...)

\method{xyzmatrix}{igraph}(x, ...)

xyzmatrix(x) <- value
}
\arguments{
  \item{x}{object containing 3d coordinates}

  \item{...}{additional arguments passed to methods}

  \item{y,z}{separate y and z coordinates}

  \item{value}{Nx3 matrix specifying new xyz coords}
}
\value{
Nx3 matrix containing 3d coordinates

Original object with modified coords
}
\description{
Get and assign coordinates for classes containing 3d vertex
data

Assign xyz elements of neuron or dotprops object. Can also
handle matrix like objects with columns named X,Y,Z
}
\examples{
n=Cell07PNs[[1]]
xyzmatrix(n)<-xyzmatrix(n)
stopifnot(isTRUE(
  all.equal(xyzmatrix(n),xyzmatrix(Cell07PNs[[1]]))
))
}
\seealso{
\code{\link{xyzmatrix}}
}

