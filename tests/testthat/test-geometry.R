context("geometry")

test_that("plane_coefficents works", {
  # From earlier PN work
  MBEntryPlane = matrix(c(70, 0, 27, -5287), ncol = 4)
  expect_equivalent(plane_coefficients(p = c(52, 0, 61), n = c(70, 0, 27)),
                    MBEntryPlane)
})

test_that("intersect_plane works", {
  # Mushroom Body Entry point in IS2 space
  mbe.is2=plane_coefficients(p=c(207, 102, 142), n=c(.6,-0.1,0.3))
  
  expect_equal(intersect_plane(Cell07PNs[[1]], plane=mbe.is2),
               c(208.623357970817, 103.73760534214, 139.33248583908))
})
