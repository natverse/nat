# tests for cmtk command line tools

if(is.null(cmtk.bindir())){
  message("skipping cmtk command line tool tests since CMTK is not installed")
} else {

context("cmtk reformatx")

#' round trip test of mat2dof/dof2mat
test_that("round trip tests for cmtk.dof2mat/cmtk.mat2dof (no shears)", {
  m=matrix(c(1.1,0,0,50,
             0,1.2,0,60,
             0,0,1.1,20,
             0,0,0,1),ncol=4,byrow=TRUE)
  tf<-tempfile(fileext='.list')
  dir.create(tf)
  on.exit(unlink(tf,recursive=TRUE))
  cmtk.mat2dof(m,f=tf)
  m2=cmtk.dof2mat(tf)
  expect_equal(m,m2,tolerance=1e-6)
})

}

test_that('cmtk.targetvolume works',{
  expect_equal(cmtk.targetvolume('target.nrrd'),shQuote('target.nrrd'))
  # Nx,Ny,Nz:dX,dY,dZ[:Ox,Oy,Oz]
  expect_equal(cmtk.targetvolume(
    read.im3d("../testdata/nrrd/LHMask.nrrd",ReadData=FALSE)),
    '--target-grid 50,50,50:1.4,1.4,1.4:0,0,0')
  expect_equal(cmtk.targetvolume(c(50,50,50,1.4,1.4,1.4)),
               '--target-grid 50,50,50:1.4,1.4,1.4')
  expect_equal(cmtk.targetvolume(c(50,50,50,1.4,1.4,1.4,0,0,0)),
               '--target-grid 50,50,50:1.4,1.4,1.4:0,0,0')
  expect_error(cmtk.targetvolume(c(50,50,50,1.4,1.4)),'Incorrect target')
})
