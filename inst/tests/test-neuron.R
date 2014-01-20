context("test core neuron methods")

test_that("Can make a neuron",{
  x=Cell07PNs[[1]]
  x2=do.call('neuron',x)
  expect_equal(x,x2)
})