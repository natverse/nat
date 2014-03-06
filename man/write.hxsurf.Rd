\name{write.hxsurf}
\alias{write.hxsurf}
\title{Write Amira surface (aka HxSurface or HyperSurface) into .surf file.}
\usage{
write.hxsurf(surf, filename)
}
\arguments{
  \item{surf}{hxsurf object to write to file.}

  \item{filename}{character vector defining path to file.}
}
\value{
\code{NULL} or integer status from \code{\link{close}}.
}
\description{
Write Amira surface (aka HxSurface or HyperSurface) into
.surf file.
}
\seealso{
\code{\link{plot3d.hxsurf}},\code{\link{read.hxsurf}},
\code{\link{rgb}}

Other amira: \code{\link{amiratype}}; \code{\link{hxsurf}},
\code{\link{read.hxsurf}}; \code{\link{is.amiramesh}};
\code{\link{read.amiralandmarks}},
\code{\link{write.amiralandmarks}};
\code{\link{read.amiramesh}},
\code{\link{read.amiramesh.header}}
}

