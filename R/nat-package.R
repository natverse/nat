#' Analyse 3D biological image data especially neurons
#' 
#' nat provides tools to read, analyse, plot, transform and convert
#' neuroanatomical data, especially representations of neurons.
#' @section neuron objects:
#' At present there are 2 main representations of neuronal data.
#' \itemize{
#' \item{\code{neuron}}{ objects contain one or more connected trees 
#' that make up a neuron}
#' \item{\code{dotprops}}{ objects each contain
#' a single neuron represented as points and tangent vectors in which the 
#' connectivity information has been discarded}
#' }
#' @section Collections of neurons:
#' Neurons can be collected as \code{\link{neuronlist}} objects, which contain
#' multiple \code{neuron} or \code{dotprops} objects along with an attached
#' dataframe of metadata that can be used to colour or subset the neurons during
#' plotting (see \code{\link{plot3d.neuronlist}}).
#' 
#' @section transformations:
#' \code{neuron} or \code{dotprops} objects can be transformed from e.g. sample 
#' to template brain space using affine or non-rigid registrations, typically 
#' calculated with the open source CMTK package (see ?\link{cmtk} for 
#' installation details). The function \code{\link{xform}} has methods to deal
#' with a variety of types of interest.
#' 
#' @section package options:
#' The following options can be set to specify default behaviour.
#' \itemize{
#' \item{\code{nat.cmtk.bindir}}{ Location of CMTK binaries. See \code{\link{cmtk.bindir}}}
#' \item{\code{nat.default.neuronlist}}{ A neuronlist to use with the \code{\link{plot3d.character}} method}
#' }
#' @name nat-package
#' @aliases nat
#' @seealso
#'   \code{\link{dotprops},\link{neuron},\link{plot3d.neuronlist},\link{xform}}, 
#'   \code{\link{rgl}} which is used for visualisation.
#' @docType package
#' @keywords package
NULL
