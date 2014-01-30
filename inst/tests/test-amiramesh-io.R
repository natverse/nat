context("amiramesh-io")

test_that("check basic reading of amiramesh header",{
  am='../testdata/neuron/EBT7R.am'
  expect_is(read.amiramesh.header(am),'list')
})

test_that("check basic reading of amiramesh file (neuron)",{
  am='../testdata/neuron/EBT7R.am'
  expect_is(read.amiramesh(am),'list')
})

test_that("check reading amiramesh in zlib and rle formats",{
  rleam='../testdata/amira/LHMask.Labels.rle.am'
  zipam='../testdata/amira/LHMask.zip.am'
  expect_is(rle<-read.amiramesh(rleam), 'array')
  expect_that(dim(rle), is_equivalent_to(c(50, 50, 50)))
  expect_is(zip<-read.amiramesh(zipam), 'array')
  expect_that(dim(zip), is_equivalent_to(c(50, 50, 50)))
  expect_equivalent(rle,zip)
})

test_that("read.zlib works in memory",{
  set.seed(42)
  raw_data=as.raw(sample(1:5,250,replace=T))
  zlib_data=memCompress(from=raw_data,type='gzip')
  expect_equal(read.zlib(zlib_data),raw_data)
})
