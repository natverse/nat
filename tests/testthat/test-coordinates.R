context("coordinates")

test_that("sub2ind returns correct indices", {
  data <- runif(120)
  data.array <- array(data=data, dim=c(4,5,6))
  subs <- mapply(function(x) which(data.array == x, arr.ind=T), data.array)
  inds <- apply(subs, 2, function(x) sub2ind(indices=x, dims=dim(data.array)))
  expect_equal(data[20:40], data[inds[20:40]])
})
