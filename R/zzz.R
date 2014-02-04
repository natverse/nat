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
    packageStartupMessage("Some nat functions depend on a CMTK installation. ",
                          "See ?cmtk and README.md for details.")
  }
  invisible()
}

# will store information about formats that we can read
.neuronformats <- new.env()