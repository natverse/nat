\name{all.equal.dotprops}
\alias{all.equal.dotprops}
\title{all.equal method tailored to dotprops objects}
\usage{
\method{all.equal}{dotprops}(target, current, check.attributes = FALSE,
  absoluteVectors = TRUE, ...)
}
\arguments{
  \item{target,current}{dotprops objects to compare}

  \item{check.attributes}{Whether to check attributes
  (false by default)}

  \item{absoluteVectors}{Whether to check only the absolute
  value of eigenvectors for equality (default TRUE, see
  details)}

  \item{...}{Additional arguments passed to base
  \code{all.equal}.}
}
\description{
all.equal method tailored to dotprops objects
}
\details{
This method is required because the direction vectors are
computed using an eigenvector decomposition where the sign
of the eigenvector is essentially random and subject to
small numerical instabilities. Therefore it does not
usually make sense to check the value of vect exactly.
}
\examples{
# equal using default
kc1=kcs20[[1]]
kc1.recalc=dotprops(kc1)
# not equal due to differences in attributes and vectors
all.equal.default(kc1.recalc, kc1)
# still not equal because of tangent vector flipping
all.equal.default(kc1.recalc, kc1, check.attributes=FALSE)
# equal using appropriate method
stopifnot(isTRUE(all.equal(kc1.recalc, kc1)))
# NB identical when recalculated on same setup from same data
stopifnot(isTRUE(all.equal.default(kc1.recalc, dotprops(kc1))))
}

