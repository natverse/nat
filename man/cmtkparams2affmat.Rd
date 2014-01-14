\name{cmtkparams2affmat}
\alias{cmtkparams2affmat}
\title{Compose homogeneous affine matrix from CMTK registration parameters}
\usage{
cmtkparams2affmat(params = NULL, tx = 0, ty = 0, tz = 0, rx = 0,
  ry = 0, rz = 0, sx = 1, sy = 1, sz = 1, shx = 0, shy = 0,
  shz = 0, cx = 0, cy = 0, cz = 0, legacy = NA)
}
\arguments{
  \item{params}{5x3 matrix of CMTK registration parameters
  or list of length 5.}

  \item{legacy}{Whether to assume that parameters are in
  the format used by CMTK <=2.4.0 (default value NA implies
  FALSE, see details).}

  \item{tx,ty,tz}{Translation along x, y and z axes
  (default 0)}

  \item{rx,ry,rz}{Rotation about x, y and z axes (in
  degrees, default 0)}

  \item{sx,sy,sz}{Scale for x, y and z axes (default 1)}

  \item{shx,shy,shz}{Shear for x,y,z axes (default 0)}

  \item{cx,cy,cz}{Centre for rotation}
}
\value{
4x4 homogeneous affine transformation matrix
}
\description{
Compose homogeneous affine matrix from CMTK registration
parameters
}
\details{
If the \code{legacy} parameter is not set explicitly, then
it will be set to \code{TRUE} if params has a version
attribute <2.4 or FALSE otherwise.
}
\seealso{
Other cmtk-geometry: \code{\link{affmat2cmtkparams}};
\code{\link{cmtk.dof2mat}}; \code{\link{cmtk.mat2dof}}
}

