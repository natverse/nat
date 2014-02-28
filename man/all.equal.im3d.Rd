\name{all.equal.im3d}
\alias{all.equal.im3d}
\title{Check equality on data and key attributes of im3d objects}
\usage{
\method{all.equal}{im3d}(target, current, tolerance = 1e-06,
  attrsToCheck = c("BoundingBox"), attrsToCheckIfPresent = c("dim", "names",
  "dimnames", "x", "y", "z"), CheckSharedAttrsOnly = FALSE, ...)
}
\arguments{
  \item{attrsToCheck}{Which attributes in im3d should
  always be checked}

  \item{attrsToCheckIfPresent}{Which attributes in im3d
  should be checked if present}

  \item{CheckSharedAttrsOnly}{Logical whether to check
  shared attributes only (default: FALSE)}

  \item{...}{additional arguments passed to
  \code{all.equal}}

  \item{target}{\R object.}

  \item{current}{other \R object, to be compared with
  \code{target}.}

  \item{tolerance}{numeric \eqn{\ge} 0.  Differences
  smaller than \code{tolerance} are not considered.}
}
\description{
Check equality on data and key attributes of im3d objects
}
\seealso{
\code{\link{all.equal}}
}

