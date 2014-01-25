\name{subset.dotprops}
\alias{subset.dotprops}
\title{Subset points in dotprops object that match given conditions}
\usage{
\method{subset}{dotprops}(x, subset, ...)
}
\arguments{
  \item{x}{A dotprops object}

  \item{subset}{A subset of points defined by indices or a
  function (see Details)}

  \item{...}{Additional parameters (currently ignored)}
}
\value{
subsetted dotprops object
}
\description{
Subset points in dotprops object that match given
conditions
}
\details{
\code{subset} defines either logical or numeric indices, in
which case these are simply applied to the matrices that
define the points, vect etc OR a function (which is called
with the 3d points array and returns T/F vector).
}
\examples{
\dontrun{
s3d=select3d()
dp1=subset(dp,s3d(points))
# special case of previous version
dp2=subset(dp,s3d)
stopifnot(all.equal(dp1,dp2))
dp2=subset(dp,alpha>0.5 & s3d(pointd))
dp3=subset(dp,1:10)
}
}
\seealso{
\code{nat.as::prune.dotprops}
}

