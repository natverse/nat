\name{threshold}
\alias{threshold}
\alias{threshold.im3d}
\title{Threshold an object, typically to produce a mask}
\usage{
threshold(x, ...)

\method{threshold}{im3d}(x, threshold = 0, mode = c("logical", "integer",
  "raw", "numeric"), ...)
}
\arguments{
  \item{x}{Object to be thresholded}

  \item{\dots}{Additional arguments passed to methods}

  \item{threshold}{Either a numeric value that pixels must
  \strong{exceed} in order to be included in the mask
  \emph{or} a \code{logical} vector defining foreground
  pixels.}

  \item{mode}{The storage mode of the resultant object (see
  \code{\link{vector}}}
}
\value{
an oject with attributes matching \code{x} and elements
with value \code{as.vector(TRUE, mode=mode)} i.e.
\code{TRUE, 1, 0x01} and \code{as.vector(FALSE, mode=mode)}
i.e. \code{FALSE, 0, 0x00} as appropriate.
}
\description{
Threshold an object, typically to produce a mask
}
\details{
Note that \code{threshold.im3d} passes \dots arguments on
to im3d
}
\examples{
x=im3d(rnorm(1000),dims=c(10,10,10), BoundingBox=c(20,200,100,200,200,300))
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

