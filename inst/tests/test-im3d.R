context("im3d")

test_that("can read im3d files",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_is(d,'array')
  expect_true(is.raw(d))
  expect_equal(sum(d!=0), 28669)
  
  expect_message(read.im3d("../testdata/nrrd/LHMask.am"),'implemented')
  expect_message(read.im3d("../testdata/nrrd/LHMask.amiramesh"),'implemented')
  expect_error(read.im3d("../testdata/nrrd/LHMask.rhubarb"))
})
