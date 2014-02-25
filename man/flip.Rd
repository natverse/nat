\name{flip}
\alias{flip}
\alias{flip.array}
\title{Flip an array, matrix or vector about an axis}
\usage{
flip(x, ...)

\method{flip}{array}(x, flipdim = "X", ...)
}
\arguments{
  \item{x}{Object to flip}

  \item{...}{Aditional arguments for methods}

  \item{flipdim}{Character vector or 1-indexed integer
  indicating array dimension alogn which flip will occur.
  Characters X, Y, Z map onto dimensions 1, 2, 3.}
}
\description{
Flip an array, matrix or vector about an axis
}
\details{
Note that dimensions 1 and 2 for R matrices will be rows
and columns, respectively, which does not map easily onto
the intuition of a 2D image matrix where the X axis would
typically be thought of as running from left to right on
the page and the Y axis would run from top to bottom.
}

