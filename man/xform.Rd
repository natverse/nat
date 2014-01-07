\name{xform}
\alias{xform}
\alias{xform.default}
\alias{xform.dotprops}
\alias{xform.list}
\alias{xform.neuronlist}
\title{Transform the 3d location of objects such as neurons}
\usage{
  xform(x, reg, ...)

  \method{xform}{default} (x, reg,
    na.action = c("warn", "none", "drop", "error"), ...)

  \method{xform}{list} (x, reg, FallBackToAffine = TRUE,
    na.action = "error", ...)

  \method{xform}{dotprops} (x, FallBackToAffine = TRUE,
    ..., k = 5)

  \method{xform}{neuronlist} (x, reg, ...)
}
\arguments{
  \item{x}{an object to transform}

  \item{reg}{an object describing a transformation in any
  of the forms understood by \code{\link{xformpoints}} (see
  details).}

  \item{...}{additional arguments passed to methods}

  \item{na.action}{How to handle NAs. NB drop may not work
  for some classes.}

  \item{FallBackToAffine}{whether to use an affine
  transform when a cmtk warping transformation fails.}

  \item{k}{Number of nearest neighbours to use for dotprops
  recalculation}
}
\description{
  \code{xform} is designed to operate on a variety of data
  types, especially objects encapsulating neurons.
}
\details{
  Methods are provided for some specialised S3 classes.
  Further methods can of course be constructed for
  user-defined S3 classes. However this will probalbly not
  be necessary if the \code{xyzmatrix} and
  \code{`xyzmatrix<-`} generics are suitably overloaded
  \emph{and} the S3 object inherits from \code{list}.

  TODO get this to work for matrices with more than 3
  columns by working on xyzmatrix definition.

  the dotprops tangent vectors will be recalculated
  post-transformation (even though they could in theory be
  transformed more or less correctly).
}
\seealso{
  xformpoints
}

