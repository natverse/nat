\name{plot3d.neuron}
\alias{plot3d.neuron}
\title{Plot neurons in 3d using rgl library}
\usage{
\method{plot3d}{neuron}(x, WithLine = TRUE, NeuronNames = FALSE,
  WithNodes = TRUE, WithAllPoints = FALSE, WithText = FALSE,
  PlotSubTrees = TRUE, add = TRUE, col = NULL, soma = FALSE, ...)
}
\arguments{
  \item{x}{A neuron to plot}

  \item{WithLine}{Whether to plot lines for all segments in
  neuron}

  \item{NeuronNames}{Logical indicating whether to label
  the neuron in the plot using the NeuronName field
  \strong{or} a character vector of names.}

  \item{WithNodes}{Whether to plot dots for branch and end
  points}

  \item{WithAllPoints}{Whether to plot dots for all points
  in the neuron}

  \item{WithText}{Whether to label plotted points with
  their id}

  \item{PlotSubTrees}{Whether to plot all sub trees when
  the neuron is not fully connected.}

  \item{add}{Whether to add the neuron to existing rgl plot
  rather than clearing the scene (default TRUE)}

  \item{col}{Colour specification (see rgl materials)}

  \item{soma}{Whether to plot a sphere at neuron's origin
  representing the soma. Either a logical value or a
  numeric indicating the radius.}

  \item{...}{Additional arguments passed to rgl::lines3d}
}
\value{
list of rgl plotting ids (invisibly) separated into
\code{lines,points,texts} according to plot element. See
\code{\link[rgl]{plot3d}} for details.
}
\description{
Plot neurons in 3d using rgl library
}
\examples{
# A new plot would have been opened if required
open3d()
plot3d(Cell07PNs[[1]],col='red')
plot3d(Cell07PNs[[2]],col='green')
# clear the current plot
plot3d(Cell07PNs[[2]],col='blue',add=FALSE)
# plot the number of all nodes
plot3d(Cell07PNs[[2]],col='red',WithText=TRUE,add=FALSE)
# include cell bodies
plot3d(Cell07PNs[3:4], col='red', soma=TRUE)
plot3d(Cell07PNs[5], col='red', soma=3)
rgl.close()
}
\seealso{
\code{\link{plot3d.neuronlist}, \link{plot3d.dotprops},
\link[rgl]{plot3d}}
}

