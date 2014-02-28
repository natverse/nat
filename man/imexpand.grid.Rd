\name{imexpand.grid}
\alias{imexpand.grid}
\title{Convert locations of im3d voxel grid into XYZ coordinates}
\usage{
imexpand.grid(d)
}
\arguments{
  \item{d}{An \code{im3d} object}
}
\value{
Nx3 matrix of image coordindates
}
\description{
Convert locations of im3d voxel grid into XYZ coordinates
}
\examples{
d=im3d(,dim=c(2,3,2),origin=c(10,20,30),voxdims=c(1,2,3))
imexpand.grid(d)
}
\seealso{
expand.grid
}

