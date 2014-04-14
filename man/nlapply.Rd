\name{nlapply}
\alias{nlapply}
\alias{nmapply}
\title{lapply and mapply for neuronlists}
\usage{
nlapply(X, FUN, ...)

nmapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
}
\arguments{
  \item{X}{A neuronlist}

  \item{FUN}{Function to be applied to each element of X}

  \item{...}{Additional arguments for FUN (see details)}

  \item{MoreArgs}{a list of other arguments to \code{FUN}.}

  \item{SIMPLIFY}{logical or character string; attempt to
  reduce the result to a vector, matrix or higher
  dimensional array; see the \code{simplify} argument of
  \code{\link{sapply}}.}

  \item{USE.NAMES}{logical; use names if the first \dots
  argument has names, or if it is a character vector, use
  that character vector as the names.}
}
\value{
A neuronlist
}
\description{
Looks after class and any attached dataframe.
}
\details{
Note that for \code{nmapply} the first argument in \dots
must be a \code{neuronlist}
}
\examples{
## nlapply example
kcs.reduced=nlapply(kcs20,function(x) subset(x,sample(nrow(x$points),50)))
open3d()
plot3d(kcs.reduced,col='red', lwd=2)
plot3d(kcs20,col='grey')
rgl.close()

## nmapply example
# flip first neuron in X, second in Y and 3rd in Z
xyzflip=nmapply(mirror, kcs20[1:3], mirrorAxis = c("X","Y","Z"),
 mirrorAxisSize=c(400,20,30))
open3d()
plot3d(kcs20[1:3])
plot3d(xyzflip)
rgl.close()
}
\seealso{
\code{\link{lapply}}

\code{\link{mapply}}

Other neuronlist: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}}; \code{\link{is.neuronlist}};
\code{\link{neuronlist}}; \code{\link{read.neurons}};
\code{\link{write.neurons}}
}

