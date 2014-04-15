\name{imslice}
\alias{imslice}
\title{Slice out a 3d subarray (or 2d matrix) from a 3d image array}
\usage{
imslice(x, slice, slicedim = "z", drop = TRUE)
}
\arguments{
  \item{x}{An im3d objet}

  \item{slice}{Indices defining the slices to keep}

  \item{slicedim}{Character vector or integer defining axis
  from which slices will be removed.}

  \item{drop}{Whether singleton dimensions will be dropped
  (default: TRUE) conveting 3d array to 2d matrix.}
}
\description{
Slice out a 3d subarray (or 2d matrix) from a 3d image
array
}
\details{
Note the sample locations stored in the x,y,z attributes
will be updated appropriately. FIXME: Should we also update
bounding box?
}
\seealso{
Other im3d: \code{\link{as.im3d}},
\code{\link{as.im3d.im3d}}; \code{\link{boundingbox}},
\code{\link{boundingbox.default}},
\code{\link{boundingbox.im3d}},
\code{\link{boundingbox<-}}; \code{\link{ijkpos}},
\code{\link{ijkpos}}, \code{\link{im3d-coords}},
\code{\link{im3d-coords}}, \code{\link{xyzpos}};
\code{\link{im3d-io}}, \code{\link{read.im3d}},
\code{\link{write.im3d}}; \code{\link{im3d}};
\code{\link{imexpand.grid}}; \code{\link{origin}};
\code{\link{projection}}; \code{\link{threshold}},
\code{\link{threshold.im3d}}; \code{\link{unmask}};
\code{\link{voxdims}}, \code{\link{voxdims.default}}
}

