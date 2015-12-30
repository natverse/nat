#' Remove plotted neurons or other 3D objects
#' 
#' The normal usage will not specify \code{x} in which case the last neurons
#' plotted by \code{plot3d.neuronlist} or any of its friends will be removed.
#' @param x rgl ids of objects to remove
#' @param slow Whether to remove neurons one by one (slowly) default: FALSE
#' @param type Type of objects to remove see \code{pop3d}.
#' @export
#' @seealso \code{\link[rgl]{pop3d}}, \code{\link{plot3d.neuronlist}}
npop3d <- function(x, slow=FALSE, type='shapes') {
  if(missing(x)){
    if(exists(".last.plot3d", envir=.plotted3d))
      x <- get(".last.plot3d", envir=.plotted3d)
    else x <- NULL
  }
  if(slow) invisible(sapply(x, function(x) try(pop3d(x, type=type))))
  else try(pop3d(unlist(x), type=type))
}
