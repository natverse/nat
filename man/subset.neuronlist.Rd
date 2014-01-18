\name{subset.neuronlist}
\alias{subset.neuronlist}
\title{Subset neuronlist returning either new neuronlist or names of chosen neurons}
\usage{
\method{subset}{neuronlist}(x, subset, INDICES = NULL,
  ReturnList = is.null(INDICES), ...)
}
\arguments{
  \item{x}{a neuronlist}

  \item{subset}{An expression that can be evaluated in the
  context of the dataframe attached to the neuronlist
  \strong{or} a function which can be applied to each
  neuron returning \code{TRUE} when that neuron should be
  included in the return list.}

  \item{INDICES}{Optional indices to subset neuronlist
  (faster for big lists).  See details.}

  \item{ReturnList}{whether to return the selected neurons
  (when T) or just their names}

  \item{...}{additional arguments passed to test function
  or subset.data.frame}
}
\value{
A \code{neuronlist} or a character vector of names when
\code{ReturnList=FALSE}.
}
\description{
Subset neuronlist returning either new neuronlist or names
of chosen neurons
}
\details{
subset \strong{either} uses the attached dataframe as the
basis of subset operation, taking the rownames of the new
dataframe to select neuronlist entries and returning that
sublist \strong{or} applies a function to every item in the
list that returns TRUE/FALSE to determine inclusion in
output list. \itemize{ \item When ReturnList is F just
return the indices into the list \item When INDICES are
specified, then use a for loop to iterate over only those
members of the list. This is equivalent to x[INDICES] but
is much faster for big lists when memory swapping occurs.
Note that any indices not present in x will be dropped with
a warning }
}
\examples{
da1pns=subset(Cell07PNs,Glomerulus=='DA1')
with(da1pns,stopifnot(all(Glomerulus=='DA1')))
gammas=subset(kcs20,type=='gamma')
with(gammas,stopifnot(all(type=='gamma')))
# define a function that checks whether a neuron has points in a region in
# space, specifically the tip of the mushroom body alpha' lobe
aptip<-function(x) {xyz=xyzmatrix(x);any(xyz[,'X']>350 & xyz[,'Y']<40)}
# this should identify the alpha'/beta' kenyon cells only
apbps=subset(kcs20,aptip)
# look at which neurons are present in the subsetted neuronlist
head(apbps)
\dontrun{
# make a 3d selection function using interactive rgl::select3d() function
s3d=select3d()
#Apply a 3d search function to the first 100 neurons in the neuronlist dataset
subset(dps[1:100],function(x) {length(subset(x,s3d))>0},ReturnList=F)
#The same but using INDICES, which is up to 100x faster when neuronlist is large
subset(dps,function(x) {length(subset(x,s3d))>0},INDICES=names(dps)[1:100])
}
}
\seealso{
\code{\link{neuronlist}, \link{subset.data.frame}}
}

