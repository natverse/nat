\name{unmask}
\alias{unmask}
\title{Make a regular im3d image array contain values at locations defined by a mask}
\usage{
unmask(x, mask, default = NA, attributes. = attributes(mask),
  copyAttributes = TRUE)
}
\arguments{
  \item{x}{the data to place on a regular grid}

  \item{mask}{An im3d regular image array where non-zero
  voxels are the selected element.}
}
\description{
Make a regular im3d image array contain values at locations
defined by a mask
}
\details{
The values in x will be placed into a grid defined by the
dimensions of the \code{mask} in the order defined by the
standard R linear subscripting of arrays (see e.g.
\code{\link{arrayInd}}).
}
\examples{
# read in a mask
LHMask=read.im3d(system.file('testdata/nrrd/LHMask.nrrd', package='nat'))
# pick out all the non zero values
inmask=LHMask[LHMask!=0]
# fill the non-zero elements of the mask with a vector that iterates over the
# values 0:9
stripes=unmask(seq(inmask)\%\%10, LHMask)
# make an image from one slice of that result array
image(imslice(stripes,11), asp=TRUE)
}
\seealso{
Other im3d: \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{im3d-io}},
\code{\link{read.im3d}}, \code{\link{write.im3d}};
\code{\link{im3d}}; \code{\link{imslice}};
\code{\link{origin}}; \code{\link{projection}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

