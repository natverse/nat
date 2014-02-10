.onLoad <- function(libname, pkgname) {
  try(cmtk.bindir(set=TRUE,check=TRUE),silent=TRUE)
  
  # Register file formats
  registerformat('swc', read=read.neuron.swc, write=write.neuron.swc, 
                 class='neuron')
  registerformat('rds', read=readRDS, write=saveRDS, class='neuron')
  registerformat('hxskel', ext='.am', read=read.neuron.hxskel, 
                 write=write.neuron.hxskel, magic=is.hxskel,
                 class='neuron', magiclen=11)
  registerformat('hxlineset', ext='.am', read=read.neuron.hxlineset, 
                 write=write.neuron.hxlineset, magic=is.hxlineset,
                 class='neuron', magiclen=11)
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
.fileformats <- new.env()