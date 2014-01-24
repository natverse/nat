\name{neuronlistfh}
\alias{as.neuronlistfh}
\alias{as.neuronlistfh.filehash}
\alias{as.neuronlistfh.neuronlist}
\alias{is.neuronlistfh}
\alias{neuronlistfh}
\title{neuronlistfh class to store multiple neurons cached on disk}
\usage{
is.neuronlistfh(nl)

as.neuronlistfh(x, df, ...)

\method{as.neuronlistfh}{neuronlist}(x, df = attr(x, "df"), ...,
  dbName = "nldb", filehash.type = "RDS")

\method{as.neuronlistfh}{filehash}(x, df, ...)
}
\arguments{
  \item{nl}{Object to test}

  \item{x}{Object to convert}

  \item{df}{Optional dataframe, where each row describes
  one neuron}

  \item{...}{Additional arguments for methods}

  \item{dbName}{The name of the underlying filehash
  database on disk}

  \item{filehash.type}{The filehash storage type}
}
\description{
\code{neuronlistfh} objects consist of a list of neuron
objects along with an optional attached dataframe
containing information about the neurons.
\code{neuronlistfh} objects also inherit from
\code{neuronlist} and therefore any appropriate methods
e.g. \code{plot3d.neuronlist} can also be used on
\code{neuronlistfh} objects.

\code{is.neuronlistfh} test if an object is a neuronlistfh

\code{as.neuronlistfh} generic function to convert an
object to neuronlistfh

\code{as.neuronlistfh.neuronlist} converts a regular
neuronlist to one backed by a filehash object with an on
disk representation

\code{as.neuronlistfh.filehash} wrap an existing filehash
object (on disk) into a neuronlistfh
}
\examples{
\dontrun{
# create on disk filehash with one file per neuron
# RDS2 is optimised for speed (especially read speed) over size
kcs20fh=as.neuronlistfh(kcs20,dbName='/path/to/my/kcdb',filehash.type='RDS2')
plot3d(subset(kcs20fh,type=='gamma'))

# in a new session
kcs20fh=neuronlist(dbName='/path/to/my/kcdb')
}
}
\seealso{
Other neuronlist: \code{\link{is.neuronlist}},
\code{\link{neuronlist}}; \code{\link{nlapply}}
}

