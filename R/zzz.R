.onLoad <- function(libname, pkgname) {
  try(cmtk.bindir(set=TRUE,check=TRUE),silent=TRUE)
  
  # Stop RGL trying to initialise a graphics device:
  # 1. if we are not running interactively AND 
  # 2. we have not been specifically instructed
  #    about this choice via the rgl.useNULL option.
  if(!interactive() && is.null(getOption('rgl.useNULL'))){
    options(rgl.useNULL=TRUE)
  }
  
  # Register file formats: neuron tracings
  registerformat('swc', read=read.neuron.swc, write=write.neuron.swc, 
                 class='neuron')
  registerformat('neuroml', ext=c('.xml','.nml'), read=read.neuron.neuroml, 
    class='neuron', magic=is.neuroml, magiclen=8L)
  registerformat('fijitraces', ext=c('.traces','.xml'), read=read.neuron.fiji, 
    class='neuron', magic=is.fijitraces, magiclen=5L)
  registerformat('rds', read=readRDS, write=saveRDS, class='neuron')
  registerformat('hxskel', ext='.am', read=read.neuron.hxskel, 
                 write=write.neuron.hxskel, magic=is.hxskel,
                 class='neuron', magiclen=14L)
  registerformat('hxlineset', ext='.am', read=read.neuron.hxlineset, 
                 write=write.neuron.hxlineset, magic=is.hxlineset,
                 class='neuron', magiclen=14L)
    
  # image formats
  registerformat('nrrd', ext=c('.nrrd','.nhdr'), read=read.im3d.nrrd, 
                 write=write.nrrd, magic=is.nrrd,
                 class='im3d', magiclen=8L)
  registerformat('amiramesh', ext=c('.am','.amiramesh'), read=read.im3d.amiramesh, 
                 write=write.amiramesh, magic=is.amiramesh.im3d,
                 class='im3d', magiclen=14L)
  registerformat('vaa3draw', ext=c('.v3d','.v3draw'), read=read.im3d.vaa3draw, 
               magic=is.vaa3draw, class='im3d', magiclen=24L)
  
  # landmarks
  registerformat('amiralandmarks', ext=c('.landmarkAscii','.landmarkBin','.am','.amiramesh'),
                 read=read.landmarks.amira, write=write.landmarks.amira,
                 magic=is.amiratype("LandmarkSet"),
                 class='landmarks', magiclen=14L)

  registerformat('fijilandmarks', ext=c('.points'), read=read.landmarks.fiji,
                 magic=is.fijilandmarks, class='landmarks', magiclen=5L)

  registerformat('cmtklandmarks', ext=c('.landmarks'), read=read.landmarks.cmtk,
                 write=write.landmarks.cmtk, magic=is.cmtklandmarks,
                 class='landmarks', magiclen=13L)

  # surfaces
  registerformat('hxsurf', ext=c('.surf', '.am','.amiramesh'), read=read.hxsurf,
                 write=write.hxsurf, magic=is.amiratype("HxSurface"),
                 class='hxsurf', magiclen=14L)
  
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

# Will store stack of plotted rgl objects, ready for popping
.plotted3d <- new.env()
