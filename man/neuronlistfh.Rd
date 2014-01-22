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
neuronlistfh objects consist of a list of neuron objects
along with an optional attached dataframe containing
information about the neurons. Relevant functions include
plot3d.neuronlistfh

\code{is.neuronlistfh} test if an object is a neuronlistfh

\code{as.neuronlistfh} generic function to convert an
object to neuronlistfh

\code{as.neuronlistfh.neuronlist} converts a regular
neuronlist to one backed by a filehash object

\code{as.neuronlistfh.filehash} wrap an existing filehash
object (on disk) into a neuronlistfh
}
\seealso{
neuronlist
}

