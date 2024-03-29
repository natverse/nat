% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neuronlistfh.R
\name{neuronlistfh}
\alias{neuronlistfh}
\alias{is.neuronlistfh}
\alias{as.neuronlistfh}
\alias{as.neuronlistfh.neuronlist}
\alias{c.neuronlistfh}
\title{neuronlistfh - List of neurons loaded on demand from disk or remote website}
\usage{
neuronlistfh(db, df, keyfilemap, hashmap = 1000L)

is.neuronlistfh(nl)

as.neuronlistfh(x, df, ...)

\method{as.neuronlistfh}{neuronlist}(
  x,
  df = attr(x, "df"),
  dbdir = NULL,
  dbClass = c("RDS", "RDS2", "DB1"),
  remote = NULL,
  WriteObjects = c("yes", "no", "missing"),
  ...
)

\method{c}{neuronlistfh}(..., recursive = FALSE)
}
\arguments{
\item{db}{a \code{filehash} object that manages an on disk database of neuron
objects. See Implementation details.}

\item{df}{Optional dataframe, where each row describes one neuron}

\item{keyfilemap}{A named character vector in which the elements are
filenames on disk (managed by the filehash object) and the names are the
keys used in R to refer to the neuron objects. Note that the keyfilemap
defines the order of objects in the neuronlist and will be used to reorder
the dataframe if necessary.}

\item{hashmap}{A logical indicating whether to add a hashed environment for
rapid object lookup by name or an integer or an integer defining a
threshold number of objects when this will happen (see Implementation
details).}

\item{nl}{Object to test}

\item{x}{Object to convert}

\item{...}{Additional arguments for methods, eventually passed to
\code{neuronlistfh()} constructor.}

\item{dbdir}{The path to the underlying \code{filehash} database on disk. For
RDS formats, by convention this should be a path whose final element is
'data' which will be turned into a directory. For DB1 format it specifies
a single file to which objects will be written.}

\item{dbClass}{The \code{filehash} database class. Defaults to \code{RDS}.}

\item{remote}{The url pointing to a remote repository containing files for
each neuron.}

\item{WriteObjects}{Whether to write objects to disk. Missing implies that
existing objects will not be overwritten. Default \code{"yes"}.}

\item{recursive}{currently ignored}
}
\value{
a \code{neuronlistfh} object which is a character \code{vector} with
  classes \code{neuronlistfh, neuronlist} and attributes \code{db, df}. See
  Implementation details.
}
\description{
\code{neuronlistfh} objects consist of a list of neuron objects
  along with an optional attached dataframe containing information about the
  neurons. In contrast to \code{neuronlist} objects the neurons are not
  present in memory but are instead dynamically loaded from disk as required.
  \code{neuronlistfh} objects also inherit from \code{neuronlist} and
  therefore any appropriate methods e.g. \code{plot3d.neuronlist} can also be
  used on \code{neuronlistfh} objects.

  \code{neuronlistfh} constructs a neuronlistfh object from a
  \code{filehash}, \code{data.frame} and \code{keyfilemap}. End users will
  \strong{not} typically use this function to make a \code{neuronlistfh}.
  They will usually read them using \code{read.neuronlistfh} and sometimes
  create them by using \code{as.neuronlistfh} on a \code{neuronlist} object.

\code{is.neuronlistfh} test if an object is a neuronlistfh

\code{as.neuronlistfh} generic function to convert an object to 
  neuronlistfh

\code{as.neuronlistfh.neuronlist} converts a regular neuronlist
  to one backed by a filehash object with an on disk representation

\code{c.neuronlistfh} adds additional neurons from one or more
  neuronlist objects to a \code{neuronlistfh} object.
}
\section{Modifying neuronlistfh objects}{
 The recommended way to do this is by
  using the \code{c.neuronlistfh} method to append one or more neuronlists to
  a neuronlistfh object. This ensures that the attached metadata for each
  data.frame is handled properly. Use as \code{nlfh <- c(nlfh, nl2)}. If you
  want to combine two \code{neuronlistfh} objects, it may make sense to
  choose the bigger one as the first-listed argument to which additional
  neurons are appended.

  There is also low-level and quite basic support for modifying neuronlistfh
  objects using the \code{[[} operator. There are two modes depending on the
  nature of the index in the assignment operation
  \code{nlfh[[index]]<-neuron}:

  \itemize{

  \item numeric index \emph{for replacement of items only}

  \item character index \emph{for replacement \bold{or} addition of items}

  }

  This distinction is because there must be a character key provided to name
  the neuron when a new one is being added, whereas an existing element can
  be referenced by position (i.e. the numeric index). Unfortunately the end
  user is responsible for manually modifying the attached data.frame when new
  neurons are added. Doing \code{nlfh[[index]]<-neuron} will do the
  equivalent of \code{attr(nlfh, 'df')[i, ]=NA} i.e. add a row containing NA
  values.
}

\section{Implementation details}{
 neuronlistfh objects are a hybrid between
  regular \code{neuronlist} objects that organise data and metadata for
  collections of neurons and a backing \code{filehash} object. Instead of
  keeping objects in memory, they are \emph{always} loaded from disk.
  Although this sounds like it might be slow, for nearly all practical
  purposes (e.g. plotting neurons) the time to read the neuron from disk is
  small compared with the time to plot the neuron; the OS will cache repeated
  reads of the same file. The benefits in memory and startup time (<1s vs
  100s for our 16,000 neuron database) are vital for collections of 1000s of
  neurons e.g. for dynamic report generation using knitr or for users with
  <8Gb RAM or running 32 bit R.

  neuronlistfh objects include: \describe{

  \item{\code{attr("keyfilemap")}}{ A named character vector that determines
  the ordering of objects in the neuronlist and translates keys in R to
  filenames on disk. For objects created by \code{as.neuronlistfh} the
  filenames will be the md5 hash of the object as calculated using
  \code{digest}. This design means that the same key can be used to refer to
  multiple distinct objects on disk. Objects are effectively versioned by
  their contents. So if an updated neuronlistfh object is posted to a website
  and then fetched by a user it will result in the automated download of any
  updated objects to which it refers.}

  \item{\code{attr("db")}}{ The backing database - typically of class
  \code{filehashRDS}. This manages the loading of objects from disk.}

  \item{\code{attr(x,"df")}}{ The data.frame of metadata which can be used to
  select and plot neurons. See \code{\link{neuronlist}} for examples.}

  \item{code{attr(x,"hashmap")}}{ (Optional) a hashed environment which can
  be used for rapid lookup using key names (rather than numeric/logical
  indices). There is a space potential to pay for this redundant lookup
  method, but it is normally worth while given that the dataframe object is
  typically considerably larger. To give some numbers, the additional
  environment might occupy ~ 1% of a 16,000 object neuronlistfh object and
  reduce mean lookup time from 0.5 ms to 1us. Having located the object, on
  my machine it can take as little as 0.1ms to load from disk, so these
  savings are relevant.}

  }

  Presently only backing objects which extend the \code{filehash} class are
  supported (although in theory other backing objects could be added). These
  include: \itemize{

  \item filehash RDS

  \item filehash RDS2 (experimental)

  \item filehash DB1 (experimental)

  }

  We have also implemented a simple remote access protocol (currently only
  for the \code{RDS} format). This allows a neuronlistfh object to be read
  from a url and downloaded to a local path. Subsequent attempts to access
  neurons stored in this list will result in automated download of the
  requested neuron to the local cache.

  An alternative backend, the experimental \code{RDS2} format is supported
  (available at \url{https://github.com/jefferis/filehash}). This is likely
  to be the most effective for large (5,000-500,000) collections of neurons,
  especially when using network filesystems (\code{NFS}, \code{AFP}) which
  are typically very slow at listing large directories.

  Finally the DB1 backend keeps the data in a single monolithic file on disk.
  This may work better when there are many small neurons (think >10,000 files
  occupying only a few GB) on NFS network file systems or Google Drive,
  neither of which are keen on having many files especially in the same
  folder. It does not allow updates from a remote location. See
  \code{\link{filehashDB1-class}} for more details.

  Note that objects are stored in a filehash, which by definition does not
  have any ordering of its elements. However neuronlist objects (like lists)
  do have an ordering. Therefore the names of a neuronlistfh object are not
  necessarily the same as the result of calling \code{names()} on the
  underlying filehash object.
}

\examples{
\dontrun{
kcnl=read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds',
'path/to/my/project/folder')
# this will automatically download the neurons from the web the first time
# it is run
plot3d(kcnl)

kcfh <- as.neuronlistfh(kcs20[1:18])
# add more neurons
kcfh <- c(kcfh, kcs20[19], kcs20[20])
# convert back to regular (in memory) neuronlist
all.equal(as.neuronlist(kcfh), kcs20)
}
\dontrun{
# create neuronlistfh object backed by filehash with one file per neuron
# by convention we create a subfolder called data in which the objects live
kcs20fh=as.neuronlistfh(kcs20, dbdir='/path/to/my/kcdb/data')
plot3d(subset(kcs20fh,type=='gamma'))
# ... and, again by convention, save the neuronlisfh object next to filehash 
# backing database
write.neuronlistfh(kcs20fh, file='/path/to/my/kcdb/kcdb.rds')

# in a new session
read.neuronlistfh("/path/to/my/kcdb/kcdb.rds")
plot3d(subset(kcs20fh, type=='gamma'))

# using the DB1 backing store (a single file on disk for all objects)
kcs20fh=as.neuronlistfh(kcs20, dbdir='/path/to/my/kcdb/kcs20fh')
# store metadata on disk
write.neuronlistfh(kcs20fh, file='/path/to/my/kcdb/kcs20fh.rds')
# read in again in a new session. You will need these two files
# kcs20fh kcs20fh.rds
kcs20fh2 <- read.neuronlistfh("/path/to/my/kcdb/kcs20fh.rds")
}
}
\seealso{
\code{\link[filehash]{filehash-class}}

Other neuronlistfh: 
\code{\link{[.neuronlistfh}()},
\code{\link{read.neuronlistfh}()},
\code{\link{remotesync}()},
\code{\link{write.neuronlistfh}()}

Other neuronlist: 
\code{\link{*.neuronlist}()},
\code{\link{is.neuronlist}()},
\code{\link{neuronlist-dataframe-methods}},
\code{\link{neuronlistz}()},
\code{\link{neuronlist}()},
\code{\link{nlapply}()},
\code{\link{read.neurons}()},
\code{\link{write.neurons}()}
}
\concept{neuronlist}
\concept{neuronlistfh}
