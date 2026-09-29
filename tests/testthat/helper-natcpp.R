# Run code on the pure R/Rvcg fallback path and, when natcpp is available,
# again on the natcpp path. Assertions inside must hold for both.
with_and_without_natcpp <- function(code) {
  code <- substitute(code)
  env <- parent.frame()
  op <- options(nat.use_natcpp=FALSE)
  on.exit(options(op))
  eval(code, env)
  options(op)
  if(use_natcpp()) eval(code, env)
  invisible()
}
