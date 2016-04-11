context("xformpoints")
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
  reg2="testdata/cmtk/dofv1.1wshears.list"
  creg2=cmtkreg(reg2)
  reg3 <- "testdata/cmtk/dofv2.4wshears.list"
  creg3 <- cmtkreg(reg3)
  xyz=matrix(1:24,ncol=3,byrow=TRUE)
  xformpoints(creg2,points=xyz)
  streamxformpoints <- matrix(c(-93.9704293, -43.3553194, -16.5924185, -91.3347853, -40.5914908, -15.2413398, -88.6991413, -37.8276622, -13.8902612, -86.0634973, -35.0638337, -12.5391826, -83.4278533, -32.3000051, -11.188104, -80.7922094, -29.5361766, -9.83702532, -78.1565654, -26.772348, -8.48594669, -75.5209214, -24.0085195, -7.13486806), ncol=3, byrow=T)

  # check that xformpoints.character dispatches ok
  expect_equal(xformpoints(creg2,points=xyz),xformpoints(reg2,points=xyz))

  # Check that xformpoints and streamxform give the same output
  expect_equal(xformpoints(creg2, points=xyz), streamxformpoints)
  
  xyz2 <- rbind(xyz, c(NA, NA, NA))
  streamxformpoints2 <- rbind(streamxformpoints, c(NA, NA, NA))
  expect_equal(xformpoints(creg2, points=xyz2), streamxformpoints2)
  
  # Check that concatenated transformations work as expected
  autoConcat <- xformpoints(c(creg2, creg3), direction=c('forward', 'inverse'), points=xyz)
  manualConcat <- xformpoints(creg3, direction='inverse', points=xformpoints(creg2, direction='forward', points=xyz))
  expect_equal(autoConcat, manualConcat, tolerance=1e-6)

  # Check direction argument recycling works
  autoConcat <- xformpoints(c(creg2, creg3), direction='forward', points=xyz)
  manualConcat <- xformpoints(creg3, direction='forward', points=xformpoints(creg2, direction='forward', points=xyz))
  expect_equal(autoConcat, manualConcat, tolerance=1e-6)
  
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
