# tests for cmtk command line tools

if(is.null(cmtk.bindir())){
  message("skipping cmtk command line tool tests since CMTK is not installed")
} else {
  
  context("cmtk reformatx")
  
  #' test handling of missing inputs (and default output file)
  expect_output(cmtk.reformatx('in.nrrd', reg='reg.list', 
                               target = 'target.nrrd'), "Missing input files.*")
  
  #' round trip test of mat2dof/dof2mat
  test_that("reformatx can reformat a volume", {
    tf=tempfile(fileext='.nrrd')
    on.exit(unlink(tf))
    expect_is(cmtk.reformatx(floating="testdata/nrrd/LHMask.nrrd",
                             target=c(10,10,10,5,5,5),
                             output=tf,
                             registrations="testdata/cmtk/dofv1.1wshears.list",
                             interpolation="nn",
                             Verbose=FALSE), "character")
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
  
  expect_error(cmtk.targetvolume(list()), 'no applicable method.*as.im3d')
  expect_error(cmtk.targetvolume(FALSE), 'Incorrect target specification')
  expect_error(cmtk.targetvolume(matrix(0,2,3)), 'Unrecognised target spec')
  
  expect_equal(cmtk.targetvolume('testdata/amira/LHMask.Labels.rle.am'),
               "--target-grid 50,50,50:1.4,1.4,1.4:95.7,60.7,0.7")
})
