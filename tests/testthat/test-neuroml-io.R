context("NeuroML input output")

test_that("read neuroml files", {
  nml_level1_files=dir("testdata/neuroml/level1", pattern = '[xn]ml$', full.names = T)
  for (f in nml_level1_files) {
    # suppress warnings re cable segments
    suppressWarnings(
      expect_is(read.neuron.neuroml(f), 'neuron', info = paste("file:",basename(f)))
    )
  }
  # suppress warnings re cable segments
  suppressWarnings(expect_is(nl<-read.neuron.neuroml(nml_level1_files[1], 
                                                     AlwaysReturnNeuronList = T),
                             'neuronlist')
  )
})


test_that("error on neuroml2 files", {
  nml2_files=dir("testdata/neuroml2", pattern = '[xn]ml$', full.names = T)
  for (f in nml2_files) {
    expect_error(read.neuron.neuroml(f))
  }
})

test_that("parse neuroml files", {
  swcs=dir("testdata/neuroml/level1", pattern = 'swc$', full.names = T)
  
  for (swc in swcs) {
    nml=paste0(tools::file_path_sans_ext(swc),".xml")
    # suppress warnings re cable segments
    suppressWarnings(
      expect_equal(read.neuron.neuroml(nml), read.neuron(swc), info = basename(nml))
    )
  }
  myidentical_graph<-function(target, current, ...){
    old_igraph = package_version(igraph::igraph.version())<'1.0'
    if(old_igraph) isTRUE(all.equal(target, current, ...))
    else igraph::identical_graphs(target, current)
  }
  suppressWarnings(
   expect_true(myidentical_graph(as.ngraph(read.morphml(nml)[[1]]), 
                                 as.ngraph(read.neuron(swc))))
  )
})

test_that("is.neuroml", {
  ff=dir(c("testdata/neuroml","testdata/neuroml2"), full.names = T, recursive = T)
  notswc=tools::file_ext(ff)!="swc"
  expect_equivalent(is.neuroml(ff), notswc)
})
