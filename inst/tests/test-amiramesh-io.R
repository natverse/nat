context("amiramesh-io")

test_that("check basic reading of amiramesh header",{
  am='../testdata/neuron/EBT7R.am'
  expect_is(read.amiramesh.header(am),'list')
})