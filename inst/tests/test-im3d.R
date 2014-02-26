context("im3d")

test_that("can read im3d files",{
  expect_is(d<-read.im3d("../testdata/nrrd/LHMask.nrrd"),'im3d')
  expect_is(d,'array')
  expect_true(is.integer(d))
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
  
  expect_is(am<-read.im3d("../testdata/amira/VerySmallLabelField.am", 
                          SimplifyAttributes=TRUE), 'im3d')
  expect_equivalent(dim(am),c(2L,2L,1L))
  expect_equal(voxdims(am),c(0.5,0.5,2))
  # somewhat oddly, Amira decides that if dim=1 for any axis, the bounding
  # box will not be 0 or infinite, but the size that would be expected for dim=2
  expect_equal(boundingbox(am),structure(c(0, 0.5, 0, 0.5, 0, 2), .Dim = 2:3))
  
  expect_is(nrrd<-read.im3d("../testdata/amira/VerySmallLabelField.nrrd",
                            SimplifyAttributes=TRUE), 'im3d')
  expect_equivalent(dim(am),c(2L,2L,1L))
  expect_equal(voxdims(am),c(0.5,0.5,2))
  
  # these should be equal when SimplifyAttributes=TRUE
  expect_equal(nrrd, am)

  expect_true(is.raw(nrrdraw<-read.im3d(ReadByteAsRaw=TRUE,
    "../testdata/amira/VerySmallLabelField.nrrd", SimplifyAttributes=TRUE,)))
  expect_true(is.raw(amraw<-read.im3d(ReadByteAsRaw=TRUE,
    "../testdata/amira/VerySmallLabelField.am", SimplifyAttributes=TRUE)))
  # ... and again
  expect_equal(nrrdraw, amraw)
})

test_that("can flip arrays",{
  m=matrix(1:4, ncol=2, nrow=2, byrow=TRUE)
  # NB the orientation is determined by matching x to 
  mf1=rbind(c(3,4),c(1,2))
  mf2=rbind(c(2,1),c(4,3))
  expect_equal(flip(m), mf1)
  expect_equal(flip(m,flipdim=2), mf2)
  expect_equal(flip(m,flipdim='y'), mf2)
  expect_error(flip(m,flipdim='z'))
  
  a6=array(1:6,1:3)
  # singleton x dimension so flip has no effect
  expect_equal(flip(a6), a6)
  expect_equal(flip(a6, 2), array(c(2,1,4,3,6,5),1:3))
  expect_equal(flip(a6, 3), array(c(5,6,3,4,1,2),1:3))
})

test_that("can slice out subarray from image",{
  i=im3d(array(1:6,1:3),voxdims=c(2,3,4))
  i2=im3d(array(1:4,c(1,2,2)),voxdims=c(2,3,4))
  expect_equal(imslice(i,1:2),i2)
})
