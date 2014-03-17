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

Other im3d: \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{ijkpos}},
\code{\link{ijkpos}}, \code{\link{im3d-coords}},
\code{\link{im3d-coords}}, \code{\link{xyzpos}};
\code{\link{im3d-io}}, \code{\link{read.im3d}},
\code{\link{write.im3d}}; \code{\link{im3d}};
\code{\link{imslice}}; \code{\link{origin}};
\code{\link{projection}}; \code{\link{threshold}},
\code{\link{threshold.im3d}}; \code{\link{unmask}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

