\name{origin}
\alias{origin}
\title{Return the space origin of a 3d image object}
\usage{
origin(x, ...)
}
\arguments{
  \item{x}{Object for which origin should be returned. See
  \code{\link{boundingbox}}.}

  \item{\dots}{Additional arguments passed to
  \code{\link{boundingbox}}}
}
\description{
Defined as the first coordinates (x,y,z) of the bounding
box, which in turn matches the nrrd definition of the
location of the "centre" of the first voxel.
}
\seealso{
Other im3d: \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{ijkpos}},
\code{\link{ijkpos}}, \code{\link{im3d-coords}},
\code{\link{im3d-coords}}, \code{\link{xyzpos}};
\code{\link{im3d-io}}, \code{\link{read.im3d}},
\code{\link{write.im3d}}; \code{\link{im3d}};
\code{\link{imexpand.grid}}; \code{\link{imslice}};
\code{\link{projection}}; \code{\link{threshold}},
\code{\link{threshold.im3d}}; \code{\link{unmask}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

