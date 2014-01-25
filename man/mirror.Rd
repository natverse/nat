\name{mirror}
\alias{mirror}
\alias{mirror.default}
\alias{mirror.neuronlist}
\title{Mirror 3d object about a given axis, optionally using a warping registration}
\usage{
mirror(x, ...)

\method{mirror}{default}(x, mirrorAxisSize, mirrorAxis = c("X", "Y", "Z"),
  warpfile = NULL, transform = c("warp", "affine", "flip"), ...)

\method{mirror}{neuronlist}(x, ...)
}
\arguments{
  \item{x}{Object with 3d points (with named cols X,Y,Z)}

  \item{...}{additional arguments passed to methods or
  eventually to \code{xform}}

  \item{mirrorAxisSize}{The bounding box of the axis to
  mirror}

  \item{mirrorAxis}{Axis to mirror (default \code{"X"}).
  Can also be an integer in range \code{1:3}.}

  \item{warpfile}{Path to (optional) CMTK registration}

  \item{transform}{whether to use warp (default) or affine
  component of registration, or simply flip about midplane
  of axis.}
}
\value{
Object with transformed points
}
\description{
Mirror 3d object about a given axis, optionally using a
warping registration
}
\details{
The warping registration can be used to account e.g. for
the asymmetry. between brain hemispheres
}
\examples{
x=Cell07PNs[[1]]
plot3d(x,col='red')
plot3d(mirror(x,168),col='green')
plot3d(mirror(x,168,transform='flip'),col='blue')
y=kcs20[[1]]
plot3d(mirror(y,564.2532,transform='flip'),col='red')
plot3d(mirror(y,mirrorAxisSize=564.2532,transform='flip'),col='blue')
}
\seealso{
\code{\link{xform}}, \code{nat.as::getBounds,
nat.as::getBoundingBox}
}

