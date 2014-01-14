\name{write.cmtkreg}
\alias{write.cmtkreg}
\title{Write out CMTK registration list to folder}
\usage{
write.cmtkreg(reglist, foldername, version = "2.4")
}
\arguments{
  \item{reglist}{List specifying CMTK registration
  parameters}

  \item{foldername}{Path to registration folder (usually
  ending in .list)}

  \item{version}{CMTK version for registration (default
  2.4)}
}
\description{
Write out CMTK registration list to folder
}
\details{
Note that transformation in the forward direction (i.e.
sample->ref) e.g. as calculated from a set of landmarks
where set 1 is the sample is considered an inverse
transformation by the IGS software. So in order to use such
a transformation as an initial affine with the registration
command the switch --initial-inverse must be used
specifying the folder name created by this function.
}
\seealso{
Other cmtk-io: \code{\link{cmtklandmarks}},
\code{\link{read.cmtklandmarks}},
\code{\link{write.cmtklandmarks}};
\code{\link{read.cmtkreg}}; \code{\link{read.cmtk}};
\code{\link{write.cmtk}}
}

