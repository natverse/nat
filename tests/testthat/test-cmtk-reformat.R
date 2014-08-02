# tests for cmtk command line tools

if(is.null(cmtk.bindir())){
  message("skipping cmtk command line tool tests since CMTK is not installed")
} else {

context("cmtk reformatx")

#' round trip test of mat2dof/dof2mat
test_that("reformatx can reformat a volume", {
  tf=tempfile(fileext='.nrrd')
  on.exit(unlink(tf))
  expect_true(cmtk.reformatx(floating="testdata/nrrd/LHMask.nrrd",
                             target=c(10,10,10,5,5,5),
                             output=tf,
                             registrations="testdata/cmtk/dofv1.1wshears.list",
                             Verbose=FALSE))
  expect_is(d<-read.im3d(tf, ReadData=FALSE), 'im3d')
  expect_equal(voxdims(d), c(5,5,5))
  expect_equal(dim(d), c(10,10,10))
})

}

test_that("cmtk.targetvolume works",{
  expect_equal(cmtk.targetvolume('target.nrrd'),shQuote('target.nrrd'))
  # Nx,Ny,Nz:dX,dY,dZ[:Ox,Oy,Oz]
  expect_equal(cmtk.targetvolume(
    read.im3d("testdata/nrrd/LHMask.nrrd",ReadData=FALSE)),
    '--target-grid 50,50,50:1.4,1.4,1.4:0,0,0')
  expect_equal(cmtk.targetvolume(c(50,50,50,1.4,1.4,1.4)),
               '--target-grid 50,50,50:1.4,1.4,1.4')
  expect_equal(cmtk.targetvolume(c(50,50,50,1.4,1.4,1.4,0,0,0)),
               '--target-grid 50,50,50:1.4,1.4,1.4:0,0,0')
  expect_error(cmtk.targetvolume(c(50,50,50,1.4,1.4)),'Incorrect target')
  
  expect_equal(cmtk.targetvolume(im3d(dims = c(50,50,50), voxdims = c(1.4,1.4,1.4))),
               cmtk.targetvolume(c(50,50,50,1.4,1.4, 1.4, 0, 0, 0)))
  
  expect_error(cmtk.targetvolume(list(), 'Incorrect target'))
})
