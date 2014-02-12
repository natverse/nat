\name{nlapply}
\alias{nlapply}
\title{lapply for neuronlists}
\usage{
nlapply(X, FUN, ...)
}
\arguments{
  \item{X}{A neuronlist}

  \item{FUN}{Function to be applied to each element of X}

  \item{...}{Additional arguments for FUN}
}
\value{
A neuronlist
}
\description{
Looks after class and any attached dataframe.
}
\examples{
kcs.flipped=nlapply(kcs20,xform,reg=function(x, ...) x*c(-1,1,1) )
open3d()
plot3d(kcs20,col='red')
plot3d(kcs.flipped,col='green')
rgl.close()
}
\seealso{
\code{\link{lapply}}

Other neuronlist: \code{\link{as.neuronlistfh}},
\code{\link{as.neuronlistfh.neuronlist}},
\code{\link{is.neuronlistfh}}, \code{\link{neuronlistfh}},
\code{\link{neuronlistfh}}; \code{\link{is.neuronlist}},
\code{\link{neuronlist}}; \code{\link{read.neurons}};
\code{\link{write.neurons}}
}

