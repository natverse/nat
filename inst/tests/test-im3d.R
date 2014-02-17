context("im3d")

test_that("can read im3d files",{
  expect_is(read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_message(read.im3d("../testdata/nrrd/LHMask.nrrd"),'implemented')
  expect_message(read.im3d("../testdata/nrrd/LHMask.nhdr"),'implemented')
  expect_message(read.im3d("../testdata/nrrd/LHMask.am"),'implemented')
  expect_message(read.im3d("../testdata/nrrd/LHMask.amiramesh"),'implemented')
  expect_error(read.im3d("../testdata/nrrd/LHMask.rhubarb"))
})
