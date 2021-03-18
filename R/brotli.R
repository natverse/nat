readBrotli <- function(x) {
  if(!requireNamespace('brotli', quietly = T))
    stop("Please install suggested brotli package for rdsb format")
  comp=readBin(x, what=raw(), n = file.size(x))
  b=brotli::brotli_decompress(comp)
  unserialize(b)
}

saveBrotli <- function(x, file, quality=2, ...) {
  if(!requireNamespace('brotli', quietly = T))
    stop("Please install suggested brotli package for rdsb format")
  b=serialize(x, NULL)
  comp=brotli::brotli_compress(b, quality=quality, ...)
  writeBin(comp, file)
  invisible(file)
}
