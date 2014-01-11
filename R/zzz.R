.onLoad <- function(libname, pkgname) {
  try(cmtk.bindir(set=TRUE,check=TRUE),silent=TRUE)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if(is.null(cmtk.bindir()))
  {
    packageStartupMessage("Full functionality of nat depends on CMTK. ",
                          "Please install and make sure that it is in your path. ",
                          "See ?cmtk for details.")
  }
  invisible()
}
