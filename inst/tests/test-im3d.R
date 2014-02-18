context("im3d")

test_that("can read im3d files",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_is(d,'array')
  expect_true(is.raw(d))
  expect_equal(sum(d!=0), 28669)
  
  expect_is(d0<-read.im3d("../testdata/nrrd/LHMask.nrrd", ReadData=FALSE),'im3d')
  expect_equal(dim(d0), dim(d))
  expect_equal(length(d0), 0L)
  
  amfile="../testdata/amira/AL-a_M.am"
  expect_is(d<-read.im3d(amfile), 'im3d')
  expect_is(d,'array')
  expect_equivalent(dim(d), c(154L, 154L, 87L))
  expect_is(d0<-read.im3d(amfile, ReadData=FALSE), 'im3d')
  expect_equivalent(dim(d0), c(154L, 154L, 87L))
  
  expect_error(read.im3d("../testdata/nrrd/LHMask.rhubarb"))
})

test_that("round trip test for im3d",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  tf=tempfile(fileext='.nrrd')
  on.exit(unlink(tf))
  
  write.im3d(d, tf, dtype='byte')
  expect_is(d2<-read.im3d(tf),'im3d')
  expect_equal(d2, d, tol=1e-6)
  tf2=tempfile(fileext='.rhubarb')
  expect_error(write.im3d(d, tf2))
})

test_that("dim, voxdims and boundingbox",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"), 'im3d')
  expect_equal(dim(d),c(50,50,50))
  
  expect_is(d0<-read.im3d("../testdata/nrrd/LHMask.nrrd", ReadData=FALSE), 'im3d')
  expect_equal(dim(d0),c(50,50,50))
  
  expect_equal(voxdims(d), c(1.4, 1.4, 1.4))
  
  bb_base=structure(c(0, 68.6, 0, 68.6, 0, 68.6), .Dim = 2:3)
  expect_equal(boundingbox(d), bb_base)
  expect_equal(boundingbox.character("../testdata/nrrd/LHMask.nrrd"), bb_base)
})
