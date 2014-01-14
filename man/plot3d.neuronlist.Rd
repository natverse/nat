\name{plot3d.neuronlist}
\alias{plot3d.character}
\alias{plot3d.neuronlist}
\title{3D plots of the elements in a neuronlist, optionally using a subset
expression}
\usage{
\method{plot3d}{neuronlist}(x, subset, col = NULL, colpal = rainbow,
  skipRedraw = 200, ...)

\method{plot3d}{character}(x, ...)
}
\arguments{
  \item{x}{a neuron list (where omitted will use MyNeurons
  as default)}

  \item{subset}{Expression evaluating to logical mask for
  neurons. See details.}

  \item{col}{An expression specifying a colour evaluated in
  the context of the dataframe attached to nl (after any
  subsetting). See details.}

  \item{colpal}{A vector of colours or a function that
  generates colours}

  \item{skipRedraw}{When plotting more than this many
  (default 200) neurons skip redraw for individual neurons
  (this is much faster for large number of neurons). Can
  also accept logical values TRUE (always skip) FALSE
  (never skip).}

  \item{...}{options passed on to plot3d (such as colours,
  line width etc)}
}
\value{
list of values of \code{plot3d} with subsetted dataframe as
attribute \code{'df'}
}
\description{
3D plots of the elements in a neuronlist, optionally using
a subset expression

\code{plot3d.character} is a convenience method intended
for exploratory work on the command line.
}
\details{
The col and subset parameters are evaluated in the context
of the dataframe attribute of the neuronlist. If col
evaluates to a factor and colpal is a named vector then
colours will be assigned by matching factor levels against
the named elements of colpal. If col evaluates to a factor
and colpal is a function then it will be used to generate
colours with the same number of levels as are used in col.

plot3d.character will check if
options('nat.default.neuronlist') has been set and then use
x as an identifier to find a neuron in that neuronlist.
}
\examples{
open3d()
plot3d(kcs20,type=='gamma',col='green')
clear3d()
plot3d(kcs20,col=type)
\dontrun{
plot3d(Cell07PNs,Glomerulus=="DA1",col='red')
plot3d(Cell07PNs,Glomerulus=="VA1d",col='green')
plot3d(Cell07PNs,Glomerulus\%in\%c("DA1",'VA1d'),
  col=c("red","green")[factor(Glomerulus)])
# the same but not specifying colours explicitly
plot3d(Cell07PNs,Glomerulus\%in\%c("DA1",'VA1d'),col=Glomerulus)
plot3d(jkn,col=sex,colpal=c(male='green',female='magenta'))
plot3d(jkn,col=cut(cVA2,20),colpal=jet.colors)
}
}

