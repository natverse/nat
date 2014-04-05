context("coordinates")

test_that("sub2ind returns correct indices", {
  data <- runif(120)
  data.array <- array(data=data, dim=c(4,5,6))
  subs <- mapply(function(x) which(data.array == x, arr.ind=T), data.array)
  inds <- apply(subs, 2, function(x) sub2ind(indices=x, dims=dim(data.array)))
  expect_equal(data[20:40], data[inds[20:40]])
})

test_that("ind2coord returns correct coordinates when given an im3d", {
  testImage <- read.im3d("testdata/nrrd/LHMask.nrrd")
  coord <- ind2coord(testImage)
  coord <- coord[1:10]
  coord.expected <- c(40.6, 42, 36.4, 37.8, 39.2, 40.6, 42, 36.4, 37.8, 39.2)
  expect_equal(coord, coord.expected)
})

test_that("coord2ind returns correct coordinates", {
  testImage <- read.im3d("testdata/nrrd/LHMask.nrrd")
  ind <- coord2ind(matrix(c(10, 20, 30, 11, 20, 30), nrow=2, byrow=TRUE), testImage)
  ind.expected <- c(53208, 53209)
  expect_equal(ind, ind.expected)
})
