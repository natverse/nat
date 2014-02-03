.onLoad <- function(libname, pkgname) {
  try(cmtk.bindir(set=TRUE,check=TRUE),silent=TRUE)
  
  # Register file formats
  neuronformats('swc',read=read.neuron.swc,class='neuron')
  neuronformats('am',read="read.neuron.hxskel",magic=is.hxskel,class='neuron')
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

# will store information about formats that we can read
.neuronformats <- new.env()