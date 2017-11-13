context("npop3d")

test_that("pop last plotted neurons",{
  nopen3d()
  original.rgl.ids=rgl.ids()
  plot3d(c("EBH11R", "EBH20L"), db=Cell07PNs)
  npop3d()
  expect_equal(rgl.ids(), original.rgl.ids)
  rgl.close()
})

context("nview3d")

test_that("nview3d can change views", {
  nopen3d()
  plot3d(c("EBH11R", "EBH20L"), db=Cell07PNs)
  x=nview3d("anterior")
  expect_true(all(c("userMatrix", "FOV") %in% names(x)))
  nview3d("anterior", extramat = rgl::rotationMatrix(pi/4, 1, 1, 0))
})
