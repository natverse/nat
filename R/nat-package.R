#' Analyse 3D biological image data especially neurons
#' 
#' \bold{nat} provides tools to read, analyse, plot, transform and convert 
#' neuroanatomical data, especially representations of neurons.
#' @section Neuron Objects: At present there are 2 main representations of 
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
#'   
#'   The \code{subset} function has both \code{\link{subset.neuron}} and 
#'   \code{\link{subset.dotprops}} methods, which can be used to keep (or 
#'   reject) specified vertices within a neuron e.g. by spatial constraints. 
#'   \code{\link{subset.neuron}} will look after the tree structure of neurons 
#'   in these circumstances.
#'   
#'   \code{neuron} objects containing connected trees can be converted to 
#'   \code{ngraph} objects, a lightweight wrapper around the 
#'   \code{\link{igraph}} library's \code{\link[igraph]{graph}} class that 
#'   preserves 3D coordinate information. This allows neurons to be manipulated 
#'   based on their graph structure, e.g. by finding all nodes upstream (closer 
#'   to the root) or downstream of a given node. The \code{\link{as.neuron}} 
#'   function can convert \code{ngraph} objects back to \code{neuron}s or 
#'   selected vertex indices can be used to subset a neuron with 
#'   \code{\link{subset.neuron}}.
#'   
#' @section Collections of Neurons: Neurons can be collected as 
#'   \code{\link{neuronlist}} objects, which contain multiple 
#'   \code{\link{neuron}} or \code{dotprops} objects along with an attached 
#'   dataframe of metadata. The metadata can be accessed and manipulated using 
#'   the \code{myneuronlist[i,j]} notation (see 
#'   \code{\link{neuronlist-dataframe-methods}}).
#'   
#'   Metadata can be used to colour or subset the neurons during plotting (see 
#'   \code{\link{plot3d.neuronlist}} and \code{\link{subset.neuronlist}}). 
#'   Interactive 3D selection of neurons in a neuronlist is also possible using 
#'   \code{\link{find.neuron}} (which makes use of rgl's \code{\link{select3d}} 
#'   function.
#'   
#'   \code{neuronlist} objects also provide additional functionality to 
#'   streamline arithmetic (e.g. scaling all the points in all neurons see 
#'   \code{\link{*.neuronlist}}) and transformations (see \bold{Transformations}
#'   section below and \code{\link{xform}}). Arbitrary functions can be applied 
#'   to each individual neuron can be applied using the \code{\link{nlapply}} 
#'   function, which also provides options for progress bars and simple 
#'   parallelisation.
#'   
#' @section Transformations: \code{\link{neuron}} or \code{\link{dotprops}} 
#'   objects can be transformed from e.g. sample to template brain space using 
#'   affine or non-rigid registrations, typically calculated with the open 
#'   source CMTK package available at \url{www.nitrc.org/projects/cmtk/}, see 
#'   ?\link{cmtk} for installation details. The function \code{\link{xform}} has
#'   methods to deal with a variety of types of interest.
#'   
#' @section 3D Image Data: In addition to data types defined by unstructured 
#'   collections of 3D vertices such as \code{\link{neuron}}, 
#'   \code{\link{dotprops}} and \code{\link{hxsurf}} objects nat provides the 
#'   \code{\link{im3d}} class to handle image/density data on a regular grid. 
#'   I/O is handled by \code{\link{read.im3d}} and \code{\link{write.im3d}}, 
#'   which are currently implemented for the amiramesh and nrrd file formats; 
#'   there is also read only access to the \href{www.vaa3d.org/}{vaa3d} raw 
#'   format.
#'   
#'   Spatial information can be queried with \code{\link{voxdims}}, 
#'   \code{\link{boundingbox}} and \code{\link{ijkpos}, \link{xyzpos}} methods. 
#'   You can convert between voxel data and coordinate (vertex) -based 
#'   representations using the following functions:
#'   
#'   \itemize{
#'   
#'   \item \code{\link{as.im3d}} The \code{as.im3d.matrix} method converts XYZ 
#'   coordinates to an \code{im3d} image volume
#'   
#'   \item \code{\link{ind2coord}} Find XYZ coordinates of specified voxels of 
#'   an \code{im3d} image volume
#'   
#'   \item \code{\link{dotprops}} The \code{dotprops.im3d} method converts an 
#'   \code{im3d} object to a \code{dotprops} format neuron, i.e. a cloud of 
#'   unconnected segments.
#'   
#'   }
#'   
#' @section Surface Data: \bold{nat} can read, write, transform and subset 
#'   surface (mesh) objects defined by Amira's HxSurface class. See 
#'   \code{\link{read.hxsurf}} and links therein. In addition hxsurf objects can
#'   be converted to the \code{\link[rgl]{mesh3d}} format, which provides a link
#'   to the \code{\link[rgl]{rgl}} package and also to packages for morphometrics
#'   and sophisticated mesh manipulation such as 
#'   \href{http://cran.r-project.org/package=Morpho}{Morpho} and
#'   \href{http://cran.r-project.org/package=Rvcg}{Rvcg}.
#'   
#' @section rgl Package: \bold{nat} uses the \bold{\code{\link[rgl]{rgl}}} package 
#'   extensively for 3D visualisation. rgl's core function is to provide 
#'   interactive visualisation (usually in an X11 window depending on OpenGL - 
#'   and therefore on a graphics card or OpenGL software emulator) but recently 
#'   significant functionality for static snapshots and embedding results in 
#'   reports such as web pages has been added. With this in mind, Duncan Murdoch
#'   has added the \code{\link[rgl]{rgl.useNULL}} option. As of nat 1.8.0, 
#'   \code{options(rgl.useNULL=TRUE)} will be set before nat is loaded in 
#'   non-interactive R sessions. If you want to use nat in interactive 
#'   environments where X11 is not available, you may want to set 
#'   \code{options(rgl.useNULL=TRUE)} manually before loading nat.
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
#' @import rgl graphics grDevices utils
NULL
