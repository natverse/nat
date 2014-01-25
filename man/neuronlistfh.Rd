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
  dbClass = c("RDS", "RDS2", "remoteDB", "localDB"), ...)

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

  \item{dbClass}{The \code{filehash} or \code{stashR}
  database class defaults to \code{filehashRDS}.}

  \item{url}{For a remoteDB object the http url for the
  remote database store.}
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

\code{as.neuronlistfh.default} wraps an existing
filehash/stashR object (with backing objects on disk) into
a neuronlistfh
}
\details{
\code{neuronlistfh} objects also inherit from
\code{neuronlist} and therefore any appropriate methods
e.g. \code{plot3d.neuronlist} can also be used on
\code{neuronlistfh} objects. Presently only backing objects
which extend the \code{filehash} class are supported. These
include: \itemize{ \item filehash RDS, RDS2 (experimental)
\item stashR remoteDB and localDB objects } The \code{RDS2}
format is experimental and only available at
\url{https://github.com/jefferis/filehash} but is likely to
be the most effective for large (>5000) collections of
neurons. the \code{remoteDB} format has the unique feature
of allowing automatic remote download of versioned neurons
from a remote repository. TODO: could be interesting if
neuronslistfh objects could wrap multiple remote
repositories.

Note that objects are stored in a filehash, which by
definition does not have any ordering of its elements.
However neuronlist objects (like lists) do have an
ordering. Therefore the names of a neuronlistfh object are
not necessarily the same as the result of names on the
underlying filehash object.

In \code{as.neuronlistfh.default} the rownames of the
dataframe determine the ordering of the objects, not the
values of \code{names()} reported by the backing database
(which does not have an intrinsic order).
}
\examples{
\dontrun{
library(stashR)
# set up local stashR cache of remote db containing 20 kenyon cells
# nb dir sets the location of the local cache and should probably be a
# sensible absolute path on your system.
kcrdb=new("remoteDB",dir='kcrdb',name='kcrdb',
  url='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/kcdb/')
kcnl=as.neuronlistfh(kcrdb)
# this will automatically download the neurons from the web the first time
# it is run
plot3d(kcnl)
}
\dontrun{
# create neuronlistfh object backed by filehash with one file per neuron
kcs20fh=as.neuronlistfh(kcs20,dbName='/path/to/my/kcdb',filehash.type='RDS')
plot3d(subset(kcs20fh,type=='gamma'))
# save neuronlisfh object next to filehash backing database
save(kcs20fh,file='/path/to/my/kcdb.rda')

# in a new session
load("/path/to/my/kcdb.rda")
plot3d(subset(kcs20fh,type=='gamma'))
}
}
\seealso{
\code{\link[stashR]{remoteDB},
\link[filehash]{filehash-class}}

Other neuronlist: \code{\link{is.neuronlist}},
\code{\link{neuronlist}}; \code{\link{nlapply}}
}

