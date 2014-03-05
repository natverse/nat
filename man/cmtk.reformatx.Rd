\name{cmtk.targetvolume}
\alias{cmtk.reformatx}
\alias{cmtk.targetvolume}
\title{Defines a target volume for a CMTK reformatx operation}
\usage{
cmtk.targetvolume(target)

cmtk.reformatx(floating, target, registrations, output, dryrun = FALSE,
  Verbose = TRUE, MakeLock = TRUE, OverWrite = c("no", "update", "yes"),
  filesToIgnoreModTimes = NULL, ...)
}
\arguments{
  \item{target}{A character vector specifying a file, an
  \code{im3d} object or a 6-or 9-vector defining a grid in
  the form Nx,Ny,Nz,dX,dY,dZ,[Ox,Oy,Oz].}

  \item{floating}{The floating image to be reformatted}

  \item{registrations}{One or more CMTK format
  registrations on disk}

  \item{output}{The output image (defaults to
  target-floating.nrrd)}

  \item{dryrun}{Just print command}

  \item{Verbose}{Whether to show cmtk status messages and
  be verbose about file update checks. Sets
  \code{reformatx} \code{--verbose} option.}

  \item{MakeLock}{Whether to use a lock file to allow
  simple parallelisation (see \code{makelock})}

  \item{OverWrite}{Whether to OverWrite an existing output
  file. One of c("no","update","yes"). When
  OverWrite='update' \code{\link{RunCmdForNewerInput}} is
  used to determine if the output is older than any of the
  input files.}

  \item{filesToIgnoreModTimes}{Input files whose
  modification time should not be checked when determining
  if new output is required.}

  \item{...}{additional arguments passed to CMTK
  \code{reformatx} after processing by
  \code{\link{cmtk.call}}.}
}
\value{
a character vector specifying the full cmtk reformatx
'--target' or '--target-grid' argument
}
\description{
Defines a target volume for a CMTK reformatx operation

Refomat an image with a CMTK registration using the
reformatx tool
}
\details{
if the character vector specifies an amiramesh file, it
will be converted to a bare \code{im3d} object and then to
an appropriate '--target-grid' specification.
}
\seealso{
\code{\link{cmtk.bindir}, \link{cmtk.call},
\link{makelock}, \link{RunCmdForNewerInput}}
}

