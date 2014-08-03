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
  reg=file.path('testdata','cmtk','dofv2.4wshears.list')
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
  reg=file.path('testdata','cmtk','dofv1.1wshears.list')
  params=matrix(c(100,50,50, 3,4,5, 1,1.1,1.2, 0.1,0.2,0.3, 0,0,0), ncol=3,
                byrow=T)
  m_base=matrix(c(0.993768017875764, 0.0124333660488193, 0.1029140991094, 
                  0, 0.0997404961300905, 1.10393643798483, 0.40556613106989, 0, 
                  0.0778012981466213, -0.0620696470929289, 1.19004163463567, 0, 
                  100, 50, 50, 1), ncol=4)
  expect_equal(cmtk.dof2mat(reg),m_base,tolerance=1e-4)
})

test_that("cmtk.call",{
  reformatx=shQuote(file.path(cmtk.bindir(check=TRUE),'reformatx'))
  expect_equal(cmtk.call('reformatx',PROCESSED.ARGS='--outfile myfile.nrrd'),
               paste(reformatx,'--outfile myfile.nrrd'))
  
  expect_equal(cmtk.call('reformatx','--outfile myfile.nrrd', mask=NULL),
               paste(reformatx,'--outfile myfile.nrrd'))
  
  expect_equal(
    cmtk.call('reformatx', PROCESSED.ARGS='--outfile myfile.nrrd', mask=TRUE),
    paste(reformatx,'--outfile myfile.nrrd','--mask'))
  
})

test_that("cmtk.statistics",{
  lhmaskfile="testdata/nrrd/LHMask.nrrd"
  statsnrrd="testdata/nrrd/dataforstats.nrrd"
  a=suppressMessages(cmtk.statistics(lhmaskfile))
  baseline_a=structure(list(min = 0, max = 1, mean = 0.22935, sdev = 0.42042, 
                            n = 125000L, Entropy = 0.53849, sum = 28669), 
                       .Names = c("min", "max", "mean", "sdev", "n", "Entropy", "sum"),
                       class = "data.frame", row.names = c(NA, -1L))
  b=cmtk.statistics(statsnrrd)
  baseline_b=structure(list(min = 0, max = 100, mean = 8e-04, sdev = 0.28284, 
                            n = 125000L, Entropy = 1e-04, sum = 100), 
                       .Names = c("min", "max", "mean", "sdev", "n", "Entropy", "sum"), 
                       class = "data.frame", row.names = c(NA, -1L))
  expect_equal(a,baseline_a)
  expect_equal(b,baseline_b)
  c=cmtk.statistics(statsnrrd,mask=lhmaskfile)
  # my hacked version of statistics provides nnz
  if('nnz'%in%names(c)){
    baseline_c=structure(list(X.M = 0:1, min = c(0, 0), max = c(0, 100), 
                              mean = c(0, 0.00349), sdev = c(0, 0.5906), 
                              n = c(96331L, 28669L), nnz = 0:1, 
                              Entropy = c(0, 0.00039), sum = c(0, 100)), 
                         .Names = c("X.M", "min", "max", "mean", "sdev", "n", "nnz", "Entropy", "sum"), 
                         class = "data.frame", row.names = c(NA, -2L))
  } else {
    baseline_c = structure(list(MaskLevel = 0:1, min = c(0, 0), max = c(0, 100), 
                                mean = c(0, 0.00349), sdev = c(0, 0.5906), 
                                n = c(96331L, 28669L), Entropy = c(0, 0.00039), 
                                sum = c(0, 100)), 
                           .Names = c("MaskLevel", "min", "max", "mean", "sdev", "n", "Entropy", "sum"), 
                           class = "data.frame", row.names = c(NA, -2L))
  }
  expect_equal(c,baseline_c)
})

}

test_that("cmtk.arg.names works",{
  expect_equal(cmtk.arg.name('mask'),'--mask')
  expect_equal(cmtk.arg.name('target.grid'),'--target-grid')
  expect_equal(cmtk.arg.name('target.offset.pixels'),'--target-offset-pixels')
})

test_that("cmtk.version works",{
  op=options(nat.cmtk.bindir="",nat.cmtk.version=NULL)
  on.exit(options(op))
  expect_true(is.na(cmtk.version()))
  expect_true(is.na(cmtk.version('1.1')))
  
  # now let's set a meaningful version and test
  options(nat.cmtk.version='3.2.1')
  expect_equal(cmtk.version(), numeric_version('3.2.1'))
  expect_true(cmtk.version('3.2.1'))
  expect_false(cmtk.version('3.2.2'))
})
