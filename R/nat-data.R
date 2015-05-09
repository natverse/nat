#' Cell07PNs: 40 Sample Projection Neurons from Jefferis, Potter et al 2007
#' 
#' These R lists (which have additional class neuronlist) contain 40 traced 
#' olfactory projection neurons from Jefferis, Potter et al 2007 that have been 
#' transformed onto the IS2 template brain (Cachero, Ostrovsky et al 2010).
#' @name Cell07PNs
#' @family nat-data
#' @seealso \code{\link{head.neuronlist}}, \code{\link{with.neuronlist}}
#' @examples
#' head(Cell07PNs)
#' table(with(Cell07PNs,Glomerulus))
#' @docType data
#' @references Jefferis G.S.X.E., Potter C.J., Chan A.M., Marin E.C., Rohlfing 
#' T., Maurer C.R.J., and Luo L. (2007). Comprehensive maps of Drosophila higher
#' olfactory centers: spatially segregated fruit and pheromone representation. 
#' Cell 128 (6), 1187--1203. 
#' \href{http://dx.doi.org/10.1016/j.cell.2007.01.040}{doi:10.1016/j.cell.2007.01.040}
#' 
#' Cachero S., Ostrovsky A.D., Yu J.Y., Dickson B.J., and Jefferis G.S.X.E. 
#' (2010). Sexual dimorphism in the fly brain. Curr Biol 20 (18), 1589--601. 
#' \href{http://dx.doi.org/10.1016/j.cub.2010.07.045}{doi:10.1016/j.cub.2010.07.045}
NULL

#' List of 20 Kenyon Cells from Chiang et al 2011 converted to dotprops objects
#' 
#' This R list (which has additional class \code{neuronlist}) contains 20 
#' skeletonized \emph{Drosophila} Kenyon cells as \code{dotprops} objects. 
#' Original data is due to Chiang et al. 2011, who have generously shared their 
#' raw data at \url{http://flycircuit.tw}. Image registration and further 
#' processing was carried out by Greg Jefferis.
#' @name kcs20
#' @family nat-data
#' @seealso \code{\link{head.neuronlist}}, \code{\link{with.neuronlist}}, 
#'   \code{\link{plot3d.neuronlist}}, \code{\link{plot3d.dotprops}},
#'   \code{\link{dotprops}}
#' @docType data
#' @references [1] Chiang A.S., Lin C.Y., Chuang C.C., Chang H.M., Hsieh C.H., 
#'   Yeh C.W., Shih C.T., Wu J.J., Wang G.T., Chen Y.C., Wu C.C., Chen G.Y., 
#'   Ching Y.T., Lee P.C., Lin C.Y., Lin H.H., Wu C.C., Hsu H.W., Huang Y.A., 
#'   Chen J.Y., et al. (2011). Three-dimensional reconstruction of brain-wide 
#'   wiring networks in Drosophila at single-cell resolution. Curr Biol 21 (1), 
#'   1--11.
#' @examples 
#' head(kcs20)
#' table(with(kcs20, type))
#' nopen3d()
#' # see plot3d.neuronlist documentation for more details
#' plot3d(kcs20, col=type)
NULL

#' Surface object (hxsurf) for the left mushroom body in FCWB template space
#' 
#' This surface object is in the same space as the 20 Kenyon cells in
#' \code{\link{kcs20}}.
#' @name MBL.surf
#' @family nat-data
#' @docType data
#' @seealso \code{\link{hxsurf}}
#' @examples 
#' plot3d(kcs20)
#' plot3d(MBL.surf, alpha=0.3)
#' \dontrun{
#' ## originally generated as follows
#' library(nat.flybrains)
#' MBL.surf=subset(FCWBNP.surf, "MB.*_L", drop = T)
#' }
NULL
