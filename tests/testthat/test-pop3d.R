context("npop3d")

test_that("pop last plotted neurons",{
  nopen3d()
  original.rgl.ids=rgl.ids()
  plot3d(c("EBH11R", "EBH20L"), db=Cell07PNs)
  npop3d()
  expect_equal(rgl.ids(), original.rgl.ids)
  rgl.close()
})
