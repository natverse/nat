# tests for cmtk command line tools

if(is.null(cmtk.bindir())){
  message("skipping cmtk command line tool tests since CMTK is not installed")
} else {

context("cmtk command line tools")

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

test_that("round trip tests for cmtk.dof2mat/cmtk.mat2dof (with shears)", {
  m=structure(c(0.911236, 0.00295678, 0.00483363, 0, -0.045134, 1.105, 
                -0.104863, 0, -0.0475781, 0.0580714, 0.997261, 0, -88.3912, -56.1266, 
                -5.21284, 1), .Dim = c(4L, 4L))
  tf<-tempfile(fileext='.list')
  dir.create(tf)
  on.exit(unlink(tf,recursive=TRUE))
  cmtk.mat2dof(m,f=tf)
  m2=cmtk.dof2mat(tf)
  expect_equal(m,m2,tolerance=1e-6)
})

test_that("test cmtk.mat2dof with shears", {
  m=structure(c(0.993768, -0.0869434, -0.0697565, 0, 0.199117, 1.08527, 
                0.0504537, 0, 0.303757, 0.211115, 1.19715, 0, 100, 50, 50, 1),
              .Dim = c(4L, 4L))
  params_base=matrix(c(100,50,50, 3,4,5, 1,1.1,1.2, 0.1,0.2,0.3, 0,0,0), ncol=3,
                     byrow=T)
  expect_equal(cmtk.mat2dof(m),params_base,tolerance=1e-4)
  expect_equal(cmtk.mat2dof(t(m),Transpose=FALSE),params_base,tolerance=1e-4)
})

test_that("test cmtk.dof2mat with v2.4 registration or in memory parameters", {
  reg=file.path('..','testdata','cmtk','dofv2.4wshears.list')
  params=matrix(c(100,50,50, 3,4,5, 1,1.1,1.2, 0.1,0.2,0.3, 0,0,0), ncol=3,
                byrow=T)
  m_base=structure(c(0.993768, -0.0869434, -0.0697565, 0, 0.199117, 1.08527, 
                     0.0504537, 0, 0.303757, 0.211115, 1.19715, 0, 100, 50, 50, 1),
                   .Dim = c(4L, 4L))
  expect_equal(cmtk.dof2mat(reg),m_base,tolerance=1e-4)
  expect_equal(cmtk.dof2mat(params),m_base,tolerance=1e-4)
  expect_equal(cmtk.dof2mat(params,Transpose=FALSE),t(m_base),tolerance=1e-4)
})

test_that("cmtk.dof2mat can compose legacy 1.1 affine parameters", {
  reg=file.path('..','testdata','cmtk','dofv1.1wshears.list')
  params=matrix(c(100,50,50, 3,4,5, 1,1.1,1.2, 0.1,0.2,0.3, 0,0,0), ncol=3,
                byrow=T)
  m_base=matrix(c(0.993768017875764, 0.0124333660488193, 0.1029140991094, 
                  0, 0.0997404961300905, 1.10393643798483, 0.40556613106989, 0, 
                  0.0778012981466213, -0.0620696470929289, 1.19004163463567, 0, 
                  100, 50, 50, 1), ncol=4)
  expect_equal(cmtk.dof2mat(reg),m_base,tolerance=1e-4)
})

}
