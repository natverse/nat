\name{neuronlistfh}
\alias{as.neuronlistfh}
\alias{as.neuronlistfh.default}
\alias{as.neuronlistfh.filehash}
\alias{as.neuronlistfh.neuronlist}
\alias{is.neuronlistfh}
\alias{neuronlistfh}
\title{neuronlistfh class to store multiple neurons cached on disk}
\usage{
is.neuronlistfh(nl)

as.neuronlistfh(x, df, ...)

\method{as.neuronlistfh}{neuronlist}(x, df = attr(x, "df"), dir = NULL,
  dbClass = c("RDS", "RDS2"), remote = NULL, ...)

\method{as.neuronlistfh}{filehash}(x, df, ...)

\method{as.neuronlistfh}{default}(x, df, ...)
}
\arguments{
  \item{nl}{Object to test}

  \item{x}{Object to convert}

  \item{df}{Optional dataframe, where each row describes
  one neuron}

  \item{...}{Additional arguments for methods}

  \item{dir}{The path to the underlying \code{filehash}
  database on disk}

  \item{dbClass}{The \code{filehash} database class.
  Defaults to \code{RDS}.}

  \item{remote}{The url pointing to a remote repository
  containing files for each neuron.}
}
\description{
\code{neuronlistfh} objects consist of a list of neuron
objects along with an optional attached dataframe
containing information about the neurons.

\code{is.neuronlistfh} test if an object is a neuronlistfh

\code{as.neuronlistfh} generic function to convert an
object to neuronlistfh

\code{as.neuronlistfh.neuronlist} converts a regular
neuronlist to one backed by a filehash object with an on
disk representation

\code{as.neuronlistfh.default} wraps an existing filehash
object (with backing objects on disk) into a neuronlistfh
}
\details{
\code{neuronlistfh} objects also inherit from
\code{neuronlist} and therefore any appropriate methods
e.g. \code{plot3d.neuronlist} can also be used on
\code{neuronlistfh} objects. Presently only backing objects
which extend the \code{filehash} class are supported. These
include: \itemize{ \item filehash RDS \item filehash RDS2
(experimental) } We have implemented a simple remote access
protocol which is currently only implemented for the
\code{RDS} format. This allows a neuronlistfh object to be
read from a url and downloaded to a local path. Subsequent
attempts to access neurons stored in this list will result
in automated download of the requested neuron to the local
cache.

The \code{RDS2} format is experimental and only available
at \url{https://github.com/jefferis/filehash} but is likely
to be the most effective for large (>5000) collections of
neurons.

Note that objects are stored in a filehash, which by
definition does not have any ordering of its elements.
However neuronlist objects (like lists) do have an
ordering. Therefore the names of a neuronlistfh object are
not necessarily the same as the result of calling
\code{names()} on the underlying filehash object.

In \code{as.neuronlistfh.default} the rownames of the
dataframe determine the ordering of the objects, not the
values of \code{names()} reported by the backing database
(which does not have an intrinsic order).
}
\examples{
\dontrun{
kcnl=read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds',
'path/to/my/project/folder')
# this will automatically download the neurons from the web the first time
# it is run
plot3d(kcnl)
}
\dontrun{
# create neuronlistfh object backed by filehash with one file per neuron
kcs20fh=as.neuronlistfh(kcs20,dir='/path/to/my/kcdb/data',filehash.type='RDS')
plot3d(subset(kcs20fh,type=='gamma'))
# save neuronlisfh object next to filehash backing database
save(kcs20fh,file='/path/to/my/kcdb/kcdb.rda')

# in a new session
load("/path/to/my/kcdb.rda")
plot3d(subset(kcs20fh,type=='gamma'))
}
}
\seealso{
\code{\link[filehash]{filehash-class}}

Other neuronlist: \code{\link{is.neuronlist}},
\code{\link{neuronlist}}; \code{\link{nlapply}}
}

