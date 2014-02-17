context("im3d")

test_that("can read im3d files",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_is(d,'array')
  expect_true(is.raw(d))
  expect_equal(sum(d!=0), 28669)
  
  amfile="../testdata/amira/AL-a_M.am"
  expect_is(d<-read.im3d(amfile), 'im3d')
  expect_is(d,'array')
  expect_equivalent(dim(d), c(154L, 154L, 87L))
  
  expect_error(read.im3d("../testdata/nrrd/LHMask.rhubarb"))
})
