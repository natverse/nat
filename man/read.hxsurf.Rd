\name{read.hxsurf}
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
S3 object of class hxsurf
}
\description{
Read Amira surface (aka HxSurface or HyperSurface) files
into hxsurf object
}
\seealso{
\code{\link{plot3d.hxsurf}}

Other amira: \code{\link{amiratype}};
\code{\link{is.amiramesh}};
\code{\link{read.amiralandmarks}},
\code{\link{write.amiralandmarks}};
\code{\link{read.amiramesh}},
\code{\link{read.amiramesh.header}}
}

