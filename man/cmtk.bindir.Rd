\name{cmtk.bindir}
\alias{cmtk}
\alias{cmtk.bindir}
\title{Return path to directory containing CMTK binaries}
\usage{
cmtk.bindir(dir = getOption("nat.cmtk.bindir"), extradirs = c("~/bin",
  "/usr/local/bin", "/opt/local/bin", "/Applications/IGSRegistrationTools/bin"),
  set = FALSE, check = FALSE, cmtktool = "gregxform")
}
\arguments{
  \item{dir}{to use as binary directory or NA (see details)
  (defaults to options('nat.cmtk.bindir')).}

  \item{extradirs}{Where to look if CMTK is not in dir or
  the PATH}

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
Queries options('nat.cmtk.bindir') if no dir is specified.
If that does not contain the appropriate binaries, it will
look in the system PATH and then a succession of plausible
places until it finds something. Setting
\code{options(nat.cmtk.bindir=NA)} or passing \code{dir=NA}
will stop the function from trying to locate CMTK, always
returning NULL unless check=TRUE when it will error out.
}
\examples{
cmtk.bindir()
# set options('nat.cmtk.bindir') according to where cmtk was found
op=options(nat.cmtk.bindir=NULL)
cmtk.bindir(set=TRUE)
options(op)
}

