context("read neurons from remote url")

test_that("we can read neuron from remote url", {
  
  vds=paste0("https://raw.githubusercontent.com/openworm/CElegansNeuroML/",
    "103d500e066125688aa7ac5eac7e9b2bb4490561/CElegans/generatedNeuroML/VD",1:2,
    ".morph.xml")
  expect_is(vdns<-read.neurons(vds), "neuronlist")
  sl=structure(list(1:6, 6:24, 24:28, c(24L, 29L, 30L, 31L, 32L, 33L, 
    34L, 35L, 36L), c(6L, 37L, 38L, 39L, 40L)), class = c("seglist", 
    "list"))
  expect_equal(vdns[[1]]$SegList, sl)
})
