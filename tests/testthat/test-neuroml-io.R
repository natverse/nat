context("NeuroML input output")

test_that("read neuroml files", {
  nml_level1_files=dir("testdata/neuroml/level1", pattern = '[xn]ml$', full.names = T)
  for (f in nml_level1_files) {
    expect_is(read.neuron.neuroml(f), 'neuron', info = paste("file:",basename(f)))
  }
})

test_that("error on neuroml2 files", {
  nml2_files=dir("testdata/neuroml2", pattern = '[xn]ml$', full.names = T)
  for (f in nml2_files) {
    expect_error(read.neuron.neuroml(f))
  }
})
