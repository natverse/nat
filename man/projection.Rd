\name{projection}
\alias{projection}
\title{Make 2D (orthogonal) projection of 3d image data}
\usage{
projection(a, projdim = "z", projfun = c("integrate", "mean", "sum"),
  na.rm = T, mask = NULL, ...)
}
\arguments{
  \item{a}{Array of image data (im3d format)}

  \item{projdim}{The image dimension down which to project}

  \item{projfun}{The function that collapses each vector of
  image data down to a single pixel. Can be a character
  vector naming a function or a function.  See details.}

  \item{na.rm}{Logical indicating whether to ignore
  \code{NA} values in the image data when calculating
  function results. default: \code{TRUE}}

  \item{mask}{A mask with the same extent as the image.}

  \item{...}{Additional arguments for projfun}
}
\description{
Make 2D (orthogonal) projection of 3d image data
}
\details{
Note that \code{projfun} must have an argument \code{na.rm}
like the S3 Summary \code{\link{groupGeneric}} functions
such as \code{sum, min} etc.

Note also that the BoundingBox of a 2d projection is not
well-defined for the axis along which the projection was
made. Presently both the evaluation location and the
BoundingBox extremes are set to 0 after a projection is
made but FIXME this is not completely satisfactory. Perhaps
defining this to be NA or the midpoint of the orginal axis
would be better justified.
}
\examples{
\dontrun{
LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
op=par(mfrow=c(1,2))
rval=image(projection(d,projfun=max))
image(projection(d,projfun=clampmax(0,10)),zlim=rval$zlim)
par(op)
}
\dontrun{
LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
image(projection(LHMask),asp=TRUE)
}
}
\seealso{
\code{\link{groupGeneric}}, \code{\link{clampmax}}

Other im3d: \code{\link{as.im3d}},
\code{\link{as.im3d.im3d}}; \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{ijkpos}},
\code{\link{ijkpos}}, \code{\link{im3d-coords}},
\code{\link{im3d-coords}}, \code{\link{xyzpos}};
\code{\link{im3d-io}}, \code{\link{read.im3d}},
\code{\link{write.im3d}}; \code{\link{im3d}};
\code{\link{imexpand.grid}}; \code{\link{imslice}};
\code{\link{origin}}; \code{\link{threshold}},
\code{\link{threshold.im3d}}; \code{\link{unmask}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

