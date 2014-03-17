\name{threshold}
\alias{threshold}
\alias{threshold.im3d}
\title{Threshold an object, typically to produce a mask}
\usage{
threshold(x, ...)

\method{threshold}{im3d}(x, threshold = 0, mode = c("logical", "integer",
  "raw", "numeric"), BoundingBox = boundingbox(x))
}
\arguments{
  \item{x}{Object to be thresholded}

  \item{\dots}{Additional arguments passed to methods}

  \item{threshold}{Either a numeric value that pixels must
  \strong{exceed} in order to be included in the mask
  \emph{or} a \code{logical} vector defining foreground
  pixels.}

  \item{mode}{The storage mode of the resultant object (see
  \code{\link{vector}})}
}
\value{
an oject with attributes matching \code{x} and the value
\code{as.vector(TRUE, mode=mode)} i.e. \code{TRUE, 1, }
}
\description{
Threshold an object, typically to produce a mask
}
\examples{
x=im3d(rnorm(1000),dim)
stopifnot(all.equal(threshold(x, 0), threshold(x, x>0)))
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
\code{\link{origin}}; \code{\link{projection}};
\code{\link{unmask}}; \code{\link{voxdims}},
\code{\link{voxdims.default}}
}

