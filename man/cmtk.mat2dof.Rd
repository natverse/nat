\name{cmtk.mat2dof}
\alias{cmtk.mat2dof}
\title{Use CMTK mat2dof to convert homogeneous affine matrix into CMTK registration}
\usage{
cmtk.mat2dof(m, f = NULL, centre = NULL, Transpose = TRUE,
  version = FALSE)
}
\arguments{
  \item{m}{Homogenous affine matrix (4x4) last row 0 0 0 1
  etc}

  \item{f}{Output file (optional)}

  \item{centre}{Centre for rotation (optional 3-vector)}

  \item{Transpose}{the input matrix so that it is read in
  as it appears on disk}

  \item{version}{When TRUE, function returns CMTK version
  number of mat2dof tool}
}
\value{
5x3 matrix of CMTK registration parameters or logical
}
\description{
Use CMTK mat2dof to convert homogeneous affine matrix into
CMTK registration
}
\details{
If no output file is supplied, 5x3 params matrix will be
returned directly. Otherwise a logical will be returned
indicating success or failure at writing to disk.
}
\seealso{
Other cmtk-commandline: \code{\link{cmtk.dof2mat}}

Other cmtk-geometry: \code{\link{affmat2cmtkparams}};
\code{\link{cmtk.dof2mat}}; \code{\link{cmtkparams2affmat}}
}

