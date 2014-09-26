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

#' @method setdiff default
#' @rdname setdiff
#' @export
setdiff.default <- function(x, y) {
  x <- as.vector(x)
  y <- as.vector(y)
  unique(if(length(x) || length(y))
    x[match(x, y, 0) == 0]
    else x)
}

#' @method setdiff neuronlist
#' @rdname setdiff
#' @export
setdiff.neuronlist <- function(x, y) {
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

#' @method union default
#' @rdname union
#' @export
union.default <- function(x, y) {
  unique(c(as.vector(x), as.vector(y)))
}

#' @method union neuronlist
#' @rdname union
#' @export
union.neuronlist <- function(x, y) {
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

#' @method intersect default
#' @rdname intersect
#' @export
intersect.default <- function(x, y) {
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0)])
}

#' @method intersect neuronlist
#' @rdname intersect
#' @export
intersect.neuronlist <- function(x, y) {
  x[names(x) %in% names(y)]
}
