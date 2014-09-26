#' Find the (asymmetric) difference between two collections of objects
#' 
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all elements
#'   of \code{x} that are not present in \code{y}.
#'   
#' @export
#' @seealso sets
setdiff <- function(x, y, ...) UseMethod("setdiff")

#' @rdname setdiff
#' @export
setdiff.default <-   function(x, y, ...) base::setdiff(x, y, ...)

#' @rdname setdiff
#' @export
setdiff.neuronlist <- function(x, y, ...) {
  x[!names(x) %in% names(y)]
}



#' Find the union of two collections of objects
#' 
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all unique elements
#'   of \code{x} and \code{y}.
#'   
#' @export
#' @seealso sets
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
#' @param x the first collection to consider.
#' @param y the second collection to consider.
#' @param ... additional arguments passed to methods
#'   
#' @return A collection of the same mode as \code{x} that contains all elements
#'   of \code{x} that are also present in \code{y}.
#'   
#' @export
#' @seealso sets
intersect <- function(x, y, ...) UseMethod("intersect")

#' @rdname intersect
#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y, ...)

#' @rdname intersect
#' @export
intersect.neuronlist <- function(x, y, ...) {
  x[names(x) %in% names(y)]
}
