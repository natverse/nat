context("Test xform")

test_that("xform can use a function to define a registration", {
  n=Cell07PNs[[1]]
  expect_equal(xform(n,reg=function(x,...) -x),n*c(-1,-1,-1))
})

test_that("xform gives same result as xformpoints", {
  reg2="../testdata/cmtk/dofv1.1wshears.list"
  creg2=cmtkreg(reg2)
  
  n=Cell07PNs[[1]]
  expect_equal(data.matrix(xform(n,reg=creg2)$d[,c("X","Y","Z")]),
               xformpoints(creg2,n$d[,c("X","Y","Z")]))
})
