\name{read.cmtklandmarks}
\alias{cmtklandmarks}
\alias{read.cmtklandmarks}
\alias{write.cmtklandmarks}
\title{Read and Write CMTK landmarks}
\usage{
read.cmtklandmarks(con)

cmtklandmarks(xyzs)

write.cmtklandmarks(xyzs, filename, Force = FALSE)
}
\arguments{
  \item{con}{Character vector specifying path or a
  connection (passed straight to \code{read.cmtk})}

  \item{xyzs}{Nx3 matrix of landmarks}

  \item{filename}{Path to write out cmtklandmarks}

  \item{Force}{Whether to overwrite an existing landmarks
  file (default FALSE)}
}
\description{
Read and Write CMTK landmarks

\code{cmtklandmarks} generates in memory list
representation of cmtk landmarks
}
\details{
CMTK landmarks are always unpaired i.e. only contain
information for one brain.
}
\seealso{
Other cmtk-io: \code{\link{read.cmtkreg}};
\code{\link{read.cmtk}}; \code{\link{write.cmtkreg}};
\code{\link{write.cmtk}}
}

