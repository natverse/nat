context("CMTK geometry")

if(is.null(cmtk.bindir())){
  cmtk_numeric_version=NULL
} else {
  cmtk_version=cmtk.dof2mat(version=TRUE)
  cmtk_numeric_version=numeric_version(sub("([0-9.]+).*",'\\1',cmtk_version))
  test_that("cMTK version is >2.4", {
    expect_true(cmtk_numeric_version>=numeric_version("2.4.0"),
                'nat depends on CMTK>=2.4.0 for affine matrix (de)composition fixes')
  })
}

checkRoundTripFromMat=function(mat,
                               test.cmtk=!is.null(cmtk_numeric_version) &&
                                 cmtk_numeric_version>=numeric_version("2.4.0")){
  params=affmat2cmtkparams(mat)
  m2=cmtkparams2affmat(params)
  expect_equal(mat,m2,tolerance=1e-5)
  if(test.cmtk){
    # repeat with cmtk tools
    params2=cmtk.mat2dof(mat)
    m3=cmtk.dof2mat(params2)
    # can't absolutely rely on getting same params back in certain degenerate
    # cases e.g. axis flip.
    # checkEqualsNumeric(params,params2,tolerance=1e-5)
    expect_equal(mat,m3,tolerance=1e-5)
  }
}

test_that("round trip works for challenging affine matrix", {
  #ReCompositionAffineShear123CentreMirrorXYZ
  params=c(100,50,10,3,4,5,-1.1,-0.9,-1,0.05,0.02,0.03,5,10,20)
  mat=cmtkparams2affmat(params)
  checkRoundTripFromMat(mat)
})

test_that("compose affine with shear works", {
  params=matrix(c(100,50,10,3,3,3,1.1,0.9,1,0.03,0.1,0.05,10,50,50),
                ncol=3,byrow=TRUE)
  affmat=matrix(c(1.09698704245255, -0.0574906547972094, -0.0575695518672382, 
                  0, 0.0794174055868413, 0.895837910136773, 0.0454677297831557, 
                  0, 0.151929624282028, -0.0103700734989691, 0.994640563641534, 
                  0, 87.462778082031, 56.3015147160819, 8.57028084743792, 1),
                ncol=4)
  expect_equal(cmtkparams2affmat(params),affmat,tolerance=1e-6)
})

test_that("decompose affine with shear works", {
  params=matrix(c(100,50,10,3,3,3,1.1,0.9,1,0.03,0.1,0.05,0,0,0),
                ncol=3,byrow=TRUE)
  attr(params,'version')=numeric_version('2.4')
  affmat=matrix(c(1.09698704245255, -0.0574906547972094, -0.0575695518672382, 
                  0, 0.0794174055868413, 0.895837910136773, 0.0454677297831557, 
                  0, 0.151929624282028, -0.0103700734989691, 0.994640563641534, 
                  0, 100, 50, 10, 1),ncol=4)
  expect_equal(affmat2cmtkparams(affmat),params,tolerance=1e-6)
})

test_that("compose affine with legacy parameters works", {
  # params equivalent to:
  # reg=file.path('..','testdata','cmtk','dofv1.1wshears.list')
  params=matrix(c(100,50,50, 3,4,5, 1,1.1,1.2, 0.1,0.2,0.3, 0,0,0), ncol=3,
                byrow=T)
  m_base=matrix(c(0.993768017875764, 0.0124333660488193, 0.1029140991094, 
                  0, 0.0997404961300905, 1.10393643798483, 0.40556613106989, 0, 
                  0.0778012981466213, -0.0620696470929289, 1.19004163463567, 0, 
                  100, 50, 50, 1), ncol=4)
  expect_equal(cmtkparams2affmat(params,legacy=TRUE),
               m_base,tolerance=1e-4)
})
