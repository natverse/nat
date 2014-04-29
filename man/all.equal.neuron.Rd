\name{all.equal.neuron}
\alias{all.equal.neuron}
\title{Check equality on key fields of neuron object}
\usage{
\method{all.equal}{neuron}(target, current, tolerance = 1e-06,
  check.attributes = FALSE, fieldsToCheck = c("NumPoints", "StartPoint",
  "BranchPoints", "EndPoints", "NumSegs", "SegList", "d"),
  fieldsToCheckIfPresent = c("NeuronName", "nTrees", "SubTrees"),
  fieldsToExclude = character(), CheckSharedFieldsOnly = FALSE, ...)
}
\arguments{
  \item{fieldsToCheck}{Which fields in the neuron are
  always check}

  \item{fieldsToCheckIfPresent}{These fields are only
  checked if they are present}

  \item{fieldsToExclude}{Character vector of fields to
  exclude from check}

  \item{CheckSharedFieldsOnly}{Logical whether to check
  shared fields only (default: FALSE)}

  \item{...}{additional arguments passed to
  \code{all.equal}}

  \item{target}{\R object.}

  \item{current}{other \R object, to be compared with
  \code{target}.}

  \item{tolerance}{numeric \eqn{\ge} 0.  Differences
  smaller than \code{tolerance} are not reported.  The
  default value is close to \code{1.5e-8}.}

  \item{check.attributes}{logical indicating if the
  \code{\link{attributes}} of \code{target} and
  \code{current} (other than the names) should be
  compared.}
}
\description{
Check equality on key fields of neuron object
}
\examples{
x=Cell07PNs[[1]]
y=x
y$NeuronName='rhubarb'
# NOT TRUE
all.equal(x, y)
# TRUE
all.equal(x, y, fieldsToExclude='NeuronName')
}
\seealso{
\code{\link{all.equal}}
}

