\name{ndigest}
\alias{ndigest}
\alias{ndigest.dotprops}
\alias{ndigest.neuronlistfh}
\title{Calculated normalised digest value for an object}
\usage{
ndigest(x, ...)

\method{ndigest}{neuronlistfh}(x, ...)

\method{ndigest}{dotprops}(x, absoluteVectors = TRUE, ...)
}
\arguments{
  \item{x}{Object for which a normalised digest will be
  computed.}

  \item{...}{Additional arguments passed to methods and
  then on to \code{\link{digest}}}

  \item{absoluteVectors}{Whether to check only the absolute
  value of eigenvectors for equality (default TRUE, see
  details)}
}
\value{
A character string containing the digest of the supplied
object computed by \code{\link{digest}}.
}
\description{
The \emph{normalised} digest should exclude any fields or
attributes irrelevant to the core contents of the object
(e.g. timestamps, absolute location of the input files on
disk etc). In theory then, this value should be constant
for the same data regardless of the particular machine on
which the digest is being computed.
}
\details{
\code{ndigest.neuronlistfh} ignores all references to local
and remote paths embedded in the \code{\link{neuronlistfh}}
object, the filehash object and any hashmap.

\code{ndigest.dotprops} ignores any \code{mtime} or
\code{file} attributes. It also converts tangent vectors to
absolute values (when \code{absoluteVectors=TRUE}) because
the direction vectors are computed using an eigenvector
decomposition where the sign of the eigenvector is
essentially random and subject to small numerical
instabilities. Therefore it does not usually make sense to
rely on the value of vect exactly.
}
\seealso{
\code{\link{digest}}

\code{\link{all.equal.dotprops}}
}

