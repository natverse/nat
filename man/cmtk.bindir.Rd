\name{cmtk.bindir}
\alias{cmtk.bindir}
\title{Return path to directory containing CMTK binaries}
\usage{
  cmtk.bindir(dir = getOption("nat.cmtk.bindir"),
    extradirs = c("~/bin", "/opt/local/bin", "/usr/local/bin", "/Applications/IGSRegistrationTools/bin"),
    set = FALSE, check = FALSE, cmtktool = "gregxform")
}
\arguments{
  \item{dir}{to use as binary directory (defaults to
  options('nat.cmtk.bindir'))}

  \item{extradirs}{Where to look if CMTK is not in dir or
  the PATH}

  \item{set}{Whether to set options('nat.cmtk.bindir') with
  the found directory}

  \item{check}{Whether to (re)check a path that has been
  set with options(nat.cmtk.bindir='/some/path')}

  \item{cmtktool}{Name of a specific cmtk tool which will
  be used to identify the location of all cmtk binaries.}
}
\description{
  Return path to directory containing CMTK binaries
}
\details{
  Queries options('nat.cmtk.bindir') if no dir is
  specified. If that does not contain the appropriate
  binaries, it will look in the system PATH and then a
  succession of plausible places until it finds something.
}

