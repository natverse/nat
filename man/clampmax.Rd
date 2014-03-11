\name{clampmax}
\alias{clampmax}
\title{Return function that finds maximum of its inputs within a clamping range}
\usage{
clampmax(xmin, xmax)
}
\arguments{
  \item{xmin,xmax}{clamping range. If xmax is missing xmin
  should be a vector of length 2.}
}
\value{
A function with signature \code{f(x, ..., na.rm)}
}
\description{
Return function that finds maximum of its inputs within a
clamping range
}
\examples{
\dontrun{
LHMask=read.im3d(system.file('tests/testthat/testdata/nrrd/LHMask.nrrd',package='nat'))
d=unmask(rnorm(sum(LHMask),mean=5,sd=5),LHMask)
op=par(mfrow=c(1,2))
rval=image(projection(d,projfun=max))
image(projection(d,projfun=clampmax(0,10)),zlim=rval$zlim)
par(op)
}
}

