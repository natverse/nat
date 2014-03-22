\name{plot.neuron}
\alias{plot.neuron}
\title{Plot a 2D project of a neuron}
\usage{
\method{plot}{neuron}(x, WithLine = TRUE, NodesOnly = TRUE,
  EPsOnly = FALSE, BPsOnly = FALSE, WithText = FALSE,
  UseCurPalette = FALSE, PlotAxes = c("XY", "YZ", "XZ", "ZY"),
  Axes = TRUE, asp = 1, MainTitle = x$NeuronName, xlim = NULL,
  ylim = NULL, AxisDirections = c(1, -1, 1), Superimpose = F,
  LineCol = NULL, PointAlpha = 1, tck = NA, lwd = par("lwd"), ...)
}
\arguments{
  \item{x}{a neuron to plot.}

  \item{WithLine}{whether to plot lines for all segments in
  neuron.}

  \item{NodesOnly}{whether points should only be drawn for
  nodes, not all points in neuron.}

  \item{EPsOnly}{whether points should only be drawn for
  end points.}

  \item{BPsOnly}{whether points should only be drawn for
  branch points.}

  \item{WithText}{whether to label plotted points with
  their id.}

  \item{UseCurPalette}{whether the current palette should
  be used for colors.}

  \item{PlotAxes}{the axes for the plot.}

  \item{Axes}{whether axes should be drawn.}

  \item{asp}{the \code{y/x} aspect ratio, see
  \code{\link{plot.window}}.}

  \item{MainTitle}{the title for the plot.}

  \item{xlim}{limits for the horizontal axis.}

  \item{ylim}{limits for the vertical axis.}

  \item{AxisDirections}{the directions for the axes. By
  default, R uses the bottom-left for the origin, whilst
  most graphics software uses the top-left. The default
  value of \code{c(1, -1, 1)} makes the produced plot
  consistent with the latter.}

  \item{Superimpose}{whether the plot should be
  superimposed on one already present.}

  \item{LineCol}{the color in which to draw the lines
  between nodes.}

  \item{PointAlpha}{the value of alpha to use in plotting
  the nodes.}

  \item{tck}{length of tick mark as fraction of plotting
  region (negative number is outside graph, positive number
  is inside, 0 suppresses ticks, 1 creates gridlines).}

  \item{lwd}{line width relative to the default
  (default=1).}

  \item{...}{additional arguments passed to plot}
}
\value{
list of plotted points (invisibly)
}
\description{
Plot a 2D project of a neuron
}
\examples{
# Draw first example neuron
plot(Cell07PNs[[1]])
# Overlay second example neuron
plot(Cell07PNs[[2]], Superimpose=TRUE)
# Clear the current plot and draw the third neuron from a different view
plot(Cell07PNs[[3]], PlotAxes="YZ")
# Just plot the end points for the fourth example neuron
plot(Cell07PNs[[4]], EPsOnly=TRUE)
}
\seealso{
\code{\link{plot3d.neuron}}
}

