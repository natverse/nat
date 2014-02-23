context("neuronlistfh remote")

test_that("Can download a neuronlistfh object with MD5'd objects", {
  localdir <- tempfile()
  dir.create(localdir)
  on.exit(unlink(localdir, recursive=TRUE))
  kcs20md5 <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds", localdir=localdir)
  expect_equal(dim(kcs20md5[[1]]$points), c(284, 3))
})
