context("Verify xformpoints methods")
require(testthat)

test_that("xformpoints can use a function to define a registration", {
  xyz=matrix(1:24,ncol=3,byrow=TRUE)
  expect_equal(xformpoints(function(x) x,points=xyz),xyz)
  expect_equal(xformpoints(function(x)-x,points=xyz),-xyz)
})

test_that("xformpoints can use a matrix to define a registration", {
  xyz=matrix(1:24,ncol=3,byrow=TRUE)
  
  # identity transform
  iaff=matrix(0,ncol=4,nrow=4); diag(iaff)=1
  expect_equal(xformpoints(iaff,points=xyz),xyz)
  
  # anisotropic scale transform
  saff=iaff; diag(saff)=c(1,2,3,1)
  expect_equal(xformpoints(saff,points=xyz),t(t(xyz)*1:3))

  # translation
  taff=iaff; taff[1:3,4]=c(50,100,20)
  expect_equal(xformpoints(taff,points=xyz),t(t(xyz)+c(50,100,20)))
})

if(!is.null(cmtk.bindir())){
test_that("xformpoints.cmtkreg works ok", {
  reg2="../testdata/cmtk/dofv1.1wshears.list"
  creg2=cmtkreg(reg2)
  xyz=matrix(1:24,ncol=3,byrow=TRUE)
  xformpoints(creg2,points=xyz)
  
  # check that xformpoints.character dispatches ok
  expect_equal(xformpoints(creg2,points=xyz),xformpoints(reg2,points=xyz))
  
  # the homogeneous affine matrix equivalent to that cmtk 1.1 registration
  m_base=matrix(c(0.993768017875764, 0.0124333660488193, 0.1029140991094, 
                  0, 0.0997404961300905, 1.10393643798483, 0.40556613106989, 0, 
                  0.0778012981466213, -0.0620696470929289, 1.19004163463567, 0, 
                  100, 50, 50, 1), ncol=4)
  # note direction = forward is required to give output equivalent to the affine
  # matrix encoded in the registration file.
  expect_equal(xformpoints(m_base,xyz),
               xformpoints(creg2,points=xyz,direction='forward'),
               tolerance=1e-6)
})
}
