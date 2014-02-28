\name{imscalebar}
\alias{imscalebar}
\title{Make a scalebar to accompany an image.im3d plot}
\usage{
imscalebar(levels, col, nlevels = NULL, zlim = NULL, horizontal = TRUE,
  lab = "Density", mar = c(4, 2, 2, 2) + 0.1, border = NULL, ...)
}
\arguments{
  \item{levels}{The levels at which z values were cut
  \strong{or} a list returned by \code{\link{imaged.im3d}}}

  \item{col}{The plotted colours for each level}

  \item{nlevels}{The number of colour levels (inferred from
  levels when \code{NULL})}

  \item{zlim}{The limits of the plotted z (intensity)
  values of the image}

  \item{horizontal}{Whether to make a horizontal or
  vertical scalebar (default: TRUE)}

  \item{lab}{The (single) axis label for the scale bar
  (default: \code{Density})}

  \item{mar}{The margins for ths plot}
}
\description{
Make a scalebar to accompany an image.im3d plot
}

