saveqs <- function(x, file, preset="high", ...) {
  if(!requireNamespace('qs', quietly = T))
    stop("Please install suggested qs package for qs format")
  qs::qsave(x, file, preset=preset, ...)
}

readqs <- function(file, ...) {
  if(!requireNamespace('qs', quietly = T))
    stop("Please install suggested qs package for qs format")
  qs::qread(file, ...)
}
