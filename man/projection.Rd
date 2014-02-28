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
}
\examples{
LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd',package='nat'))
image(projection(LHMask),asp=TRUE)
}
\seealso{
\code{\link{groupGeneric}}

Other im3d: \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}}; \code{\link{im3d-io}},
\code{\link{read.im3d}}, \code{\link{write.im3d}};
\code{\link{im3d}}; \code{\link{voxdims}},
\code{\link{voxdims.default}}
}

