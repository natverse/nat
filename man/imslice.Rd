\name{imslice}
\alias{imslice}
\title{Slice out a 3d subarray (or 2d matrix) from a 3d image array}
\usage{
imslice(x, slice, slicedim = "z", drop = TRUE)
}
\arguments{
  \item{x}{An im3d objet}

  \item{slice}{Indices defining the slices to keep}

  \item{slicedim}{Charaacter vector or integer defining
  axis from which slices will be removed.}

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

