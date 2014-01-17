\name{xform}
\alias{xform}
\alias{xform.default}
\alias{xform.dotprops}
\alias{xform.list}
\alias{xform.neuronlist}
\title{Transform the 3d location of objects such as neurons}
\usage{
xform(x, reg, ...)

\method{xform}{default}(x, reg, na.action = c("warn", "none", "drop",
  "error"), ...)

\method{xform}{list}(x, reg, FallBackToAffine = TRUE, na.action = "error",
  ...)

\method{xform}{dotprops}(x, reg, FallBackToAffine = TRUE, ...)

\method{xform}{neuronlist}(x, reg, ...)
}
\arguments{
  \item{x}{an object to transform}

  \item{reg}{an object describing a transformation in any
  of the forms understood by \code{\link{xformpoints}} (see
  details).}

  \item{...}{additional arguments passed to methods and
  eventually to \code{\link{xformpoints}}}

  \item{na.action}{How to handle NAs. NB drop may not work
  for some classes.}

  \item{FallBackToAffine}{Whether to use an affine
  transform when a cmtk warping transformation fails.}
}
\description{
\code{xform} is designed to operate on a variety of data
types, especially objects encapsulating neurons.
}
\details{
Methods are provided for some specialised S3 classes.
Further methods can of course be constructed for
user-defined S3 classes. However this will probalbly not be
necessary if the \code{xyzmatrix} and \code{`xyzmatrix<-`}
generics are suitably overloaded \emph{and} the S3 object
inherits from \code{list}.

TODO get this to work for matrices with more than 3 columns
by working on xyzmatrix definition.

the dotprops tangent vectors will be recalculated after the
points have been transformed (even though they could in
theory be transformed more or less correctly).
}
\examples{
\dontrun{
kc1=kcs20[[1]]
kc1.default=xform(kc1,function(x,...) x)
stopifnot(isTRUE(all.equal(kc1,kc1.default)))
kc1.5=xform(kc1,function(x,...) x, k=5)
stopifnot(isTRUE(all.equal(kc1.5,kc1.default)))
kc1.20=xform(kc1,function(x,...) x, k=20)
stopifnot(!isTRUE(all.equal(kc1,kc1.20)))
}
}
\seealso{
\code{\link{xformpoints}}
}

