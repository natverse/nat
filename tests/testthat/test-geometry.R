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

test_that("intersect plane works with neurons with unsual node numbering", {
  # LH Entry point
  p = structure(c(250.498678796513, 95.7356097950324, 140.205235323248),
                .Names = c("X", "Y", "Z"))
  n = structure(
    c(0.770958060160371, 0.0341727609563214, -0.411977026375333),
    .Names = c("X", "Y", "Z")
  )
  lhe.is2 = plane_coefficients(p, n)
  
  # make longest path from root
  spines13 = nlapply(Cell07PNs[1:3], spine, UseStartPoint = T)
  
  baseline = structure(
    c(
      249.569781195373,
      96.8351026741921,
      138.558132834791,
      249.735593779607,
      96.4402061745495,
      138.835672205403,
      249.73484026201,
      95.764983404868,
      138.778253574627
    ),
    .Dim = c(3L, 3L),
    .Dimnames = list(NULL, c("EBH11R", "EBH20L", "EBH20R"))
  )
  expect_equal(sapply(Cell07PNs[1:3], intersect_plane, lhe.is2, closestpoint = p),
               baseline)
  
  expect_equal(sapply(spines13, intersect_plane, lhe.is2, closestpoint = p),
               baseline)
})
