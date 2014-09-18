#' Analyse 3D biological image data especially neurons
#' 
#' nat provides tools to read, analyse, plot, transform and convert 
#' neuroanatomical data, especially representations of neurons.
#' @section neuron objects: At present there are 2 main representations of 
#'   neuronal data:
#'   
#'   \itemize{
#'   
#'   \item{\code{\link{neuron}}}{ objects contain one or more connected trees 
#'   that make up a neuron}
#'   
#'   \item{\code{\link{dotprops}}}{ objects can contain one (or more) neurons 
#'   represented as points and tangent vectors in which the connectivity 
#'   information has been discarded}
#'   
#'   }
#' @section Collections of Neurons: Neurons can be collected as 
#'   \code{\link{neuronlist}} objects, which contain multiple \code{neuron} or 
#'   \code{dotprops} objects along with an attached dataframe of metadata that 
#'   can be used to colour or subset the neurons during plotting (see 
#'   \code{\link{plot3d.neuronlist}} and \code{\link{subset.neuronlist}}). 
#'   Interactive 3D selection of neurons in a neuronlist is also possible using 
#'   \code{\link{find.neuron}} (which makes use of rgl's \code{\link{select3d}} 
#'   function.
#'   
#' @section Transformations: \code{neuron} or \code{dotprops} objects can be 
#'   transformed from e.g. sample to template brain space using affine or 
#'   non-rigid registrations, typically calculated with the open source CMTK 
#'   package available at \url{www.nitrc.org/projects/cmtk/}, see ?\link{cmtk} 
#'   for installation details. The function \code{\link{xform}} has methods to 
#'   deal with a variety of types of interest.
#'   
#' @section 3d Image Data: In addition to data types defined by unstructured 
#'   collections of 3d vertices such as \code{\link{neuron}, \link{dotprops}} 
#'   and \code{\link{hxsurf}} objects nat provides the \code{\link{im3d}} class 
#'   to handle image/density data on a regular grid. I/O is handled by 
#'   \code{\link{read.im3d}} and \code{\link{write.im3d}}, which are currently 
#'   implemented for the amiramesh and nrrd file formats. Spatial information 
#'   can be queried with \code{\link{voxdims}}, \code{\link{boundingbox}} and 
#'   \code{\link{ijkpos}, \link{xyzpos}} methods.
#'   
#' @section Package Options: The following options can be set to specify default
#'   behaviour.
#'   
#'   \itemize{
#'   
#'   \item{\code{nat.cmtk.bindir}}{ Location of CMTK binaries. See 
#'   \code{\link{cmtk.bindir}}}
#'   
#'   \item{\code{nat.default.neuronlist}}{ A neuronlist to use with the 
#'   \code{\link{plot3d.character}} method}
#'   
#'   }
#'   
#'   In addition there is one read-only option: \itemize{
#'   
#'   \item \code{nat.cmtk.version} which is used to store the current cmtk 
#'   version when there are repeated calls to \code{\link{cmtk.version}}.
#'   
#'   }
#' @name nat-package
#' @aliases nat
#' @seealso 
#' \code{\link{dotprops},\link{neuron},\link{plot3d.neuronlist},\link{xform}}, 
#' \code{\link{rgl}} which is used for visualisation.
#' @docType package
#' @keywords package
#' @import rgl
NULL
