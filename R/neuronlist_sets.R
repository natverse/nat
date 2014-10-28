#' Find the (asymmetric) difference between two collections of objects
#' 
#' @details Note that setdiff.default calls base::setdiff to ensure consistent 
#'   behaviour for regular vectors.
#'   
#'   As a convenience \code{setdiff.neuronlist} allows \code{y}, the second
#'   collection, to be a character vector of names.
#'   
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all elements 
#'   of \code{x} that are not present in \code{y}.
#'   
#' @export
#' @seealso \code{\link[base]{setdiff}}
setdiff <- function(x, y, ...) UseMethod("setdiff")

#' @rdname setdiff
#' @export
setdiff.default <-   function(x, y, ...) base::setdiff(x, y, ...)

#' @rdname setdiff
#' @export
setdiff.neuronlist <- function(x, y, ...) {
  if (is.neuronlist(y)) {
    y<-names(y)
  }
  if(!is.character(y))
    stop("y must be a neuronlist or a character vector of names!")
  x[!names(x) %in% y]
}



#' Find the union of two collections of objects
#' 
#' @details Note that union.default calls base::union to ensure consistent 
#'   behaviour for regular vectors.
#'   
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all unique
#'   elements of \code{x} and \code{y}.
#'   
#' @export
#' @seealso \code{\link[base]{union}}
union <- function(x, y, ...) UseMethod("union")

#' @rdname union
#' @export
union.default <-     function(x, y, ...) base::union(x, y, ...)

#' @rdname union
#' @export
union.neuronlist <- function(x, y, ...) {
  c(x, y[!names(y) %in% names(x)])
}



#' Find the intersection of two collections of objects
#' 
#' @details Note that intersect.default calls base::intersect to ensure
#'   consistent behaviour for regular vectors.
#'   
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all elements 
#'   of \code{x} that are also present in \code{y}.
#'   
#' @export
#' @seealso \code{\link[base]{intersect}}
intersect <- function(x, y, ...) UseMethod("intersect")

#' @rdname intersect
#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y, ...)

#' @rdname intersect
#' @export
intersect.neuronlist <- function(x, y, ...) {
  x[names(x) %in% names(y)]
}
