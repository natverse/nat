\name{cmtk.bindir}
\alias{cmtk}
\alias{cmtk.bindir}
\title{Return path to directory containing CMTK binaries}
\usage{
cmtk.bindir(firstdir = getOption("nat.cmtk.bindir"), extradirs = c("~/bin",
  "/usr/local/lib/cmtk/bin", "/usr/local/bin", "/opt/local/bin",
  "/Applications/IGSRegistrationTools/bin"), set = FALSE, check = FALSE,
  cmtktool = "gregxform")
}
\arguments{
  \item{firstdir}{Character vector specifying path
  containing CMTK binaries or NA (see details). This
  defaults to options('nat.cmtk.bindir').}

  \item{extradirs}{Where to look if CMTK is not in
  \code{firstdir} or the PATH}

  \item{set}{Whether to set options('nat.cmtk.bindir') with
  the found directory}

  \item{check}{Whether to (re)check that a path that has
  been set appropriately in
  options(nat.cmtk.bindir='/some/path') or now found in the
  PATH or alternative directories. Will throw an error on
  failure.}

  \item{cmtktool}{Name of a specific cmtk tool which will
  be used to identify the location of all cmtk binaries.}
}
\value{
Character vector giving path to CMTK binary directory or
NULL when this cannot be found.
}
\description{
Return path to directory containing CMTK binaries
}
\details{
Queries options('nat.cmtk.bindir') if \code{firtsdir} is
not specified. If that does not contain the appropriate
binaries, it will look in the system PATH and then a
succession of plausible places until it finds something.
Setting \code{options(nat.cmtk.bindir=NA)} or passing
\code{firstdir=NA} will stop the function from trying to
locate CMTK, always returning NULL unless check=TRUE when
it will error out.
}
\section{Installation}{
  It is recommended to install released CMTK versions
  available from the
  \href{www.nitrc.org/projects/cmtk/}{NITRC website}. A bug
  in composition of affine transformations from CMTK
  parameters in the CMTK versions <2.4 series means that
  CMTK>=3.0 is strongly recommended.  CMTK v3 registrations
  are not backwards compatible with CMTK v2, but CMTKv3 can
  correctly interpret and convert registrations from
  earlier versions.
}
\examples{
cmtk.bindir()
\dontrun{
# set options('nat.cmtk.bindir') according to where cmtk was found
op=options(nat.cmtk.bindir=NULL)
cmtk.bindir(set=TRUE)
options(op)
}
}

