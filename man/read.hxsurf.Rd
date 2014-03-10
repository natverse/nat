\name{read.hxsurf}
\alias{hxsurf}
\alias{read.hxsurf}
\title{Read Amira surface (aka HxSurface or HyperSurface) files into hxsurf object}
\usage{
read.hxsurf(filename, RegionNames = NULL, RegionChoice = "Inner",
  FallbackRegionCol = "grey", Verbose = FALSE)
}
\arguments{
  \item{filename}{Character vector defining path to file}

  \item{RegionNames}{Character vector specifying which
  regions should be read from file. Default value of
  \code{NULL} => all regions.}

  \item{RegionChoice}{Whether the \emph{Inner} or
  \emph{Outer} material should define the material of the
  patch.}

  \item{FallbackRegionCol}{Colour to set regions when no
  colour is defined}

  \item{Verbose}{Print status messages during parsing when
  \code{TRUE}}
}
\value{
A list with S3 class hxsurf with elements \itemize{

\item{Vertices}{ A data.frame with columns \code{X, Y, Z,
PointNo}}

\item{Regions}{ A list with 3 column data.frames specifying
triplets of vertices for each region (with reference to
\code{PointNo} column in \code{Vertices} element)}

\item{RegionList}{ Character vector of region names (should
match names of \code{RegionList} element)}

\item{RegionColourList}{ Character vector specifying
default colour to plot each region in R's \code{\link{rgb}}
format}

}
}
\description{
Read Amira surface (aka HxSurface or HyperSurface) files
into hxsurf object
}
\seealso{
\code{\link{plot3d.hxsurf}, \link{rgb}}

Other amira: \code{\link{amiratype}};
\code{\link{is.amiramesh}};
\code{\link{read.amiralandmarks}},
\code{\link{write.amiralandmarks}};
\code{\link{read.amiramesh}},
\code{\link{read.amiramesh.header}};
\code{\link{write.hxsurf}}
}

