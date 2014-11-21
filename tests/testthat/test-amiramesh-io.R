context("amiramesh-io")

test_that("check basic reading of amiramesh header",{
  am='testdata/neuron/EBT7R.am'
  expect_is(read.amiramesh.header(am),'list')
})

test_that("check basic reading of amiramesh file (neuron)",{
  am='testdata/neuron/EBT7R.am'
  expect_is(read.amiramesh(am),'list')
})

test_that("check reading amiramesh in zlib and rle formats",{
  rleam='testdata/amira/LHMask.Labels.rle.am'
  zipam='testdata/amira/LHMask.zip.am'
  expect_is(rle<-read.amiramesh(rleam), 'array')
  expect_that(dim(rle), is_equivalent_to(c(50, 50, 50)))
  expect_is(zip<-read.amiramesh(zipam), 'array')
  expect_that(dim(zip), is_equivalent_to(c(50, 50, 50)))
  expect_equivalent(rle,zip)
})

test_that("we can avoid reading data sections",{
  zipam='testdata/amira/LHMask.zip.am'
  expect_is(d<-read.amiramesh(zipam), 'array')
  expect_equal(attr(d,'BoundingBox'), c(95.7, 164.3, 60.7, 129.3, 0.7, 69.3))
})

test_that("read.zlib works in memory",{
  set.seed(42)
  raw_data=as.raw(sample(1:5,250,replace=T))
  zlib_data=memCompress(from=raw_data,type='gzip')
  expect_equal(read.zlib(zlib_data),raw_data)
})

test_that("read.amiramesh can read an 8 bit amira file",{
  amfile="testdata/amira/AL-a_M.am"
  x=read.amiramesh(amfile)
  # count non-zero elements
  counts=c(6623L, 3304L, 2046L, 1529L, 1257L, 1054L, 907L, 706L, 657L, 
           557L, 458L, 444L, 399L, 325L, 307L, 247L, 269L, 224L, 185L, 196L, 
           186L, 147L, 150L, 146L, 120L, 138L, 122L, 105L, 94L, 95L, 90L, 
           86L, 88L, 81L, 88L, 66L, 63L, 78L, 53L, 76L, 53L, 43L, 47L, 49L, 
           54L, 46L, 42L, 43L, 33L, 38L, 37L, 28L, 26L, 19L, 29L, 33L, 21L, 
           30L, 21L, 25L, 12L, 19L, 20L, 19L, 8L, 14L, 14L, 15L, 10L, 16L, 
           14L, 15L, 9L, 9L, 13L, 10L, 8L, 9L, 12L, 6L, 6L, 8L, 7L, 9L, 
           9L, 7L, 7L, 3L, 6L, 9L, 9L, 5L, 7L, 6L, 6L, 3L, 6L, 5L, 7L, 10L, 
           8L, 4L, 5L, 2L, 3L, 4L, 4L, 6L, 1L, 6L, 4L, 1L, 6L, 0L, 6L, 10L, 
           0L, 6L, 2L, 4L, 3L, 4L, 4L, 4L, 3L, 6L, 1L, 3L, 2L, 3L, 9L, 2L, 
           1L, 3L, 3L, 1L, 2L, 4L, 2L, 3L, 6L, 2L, 4L, 2L, 2L, 4L, 0L, 1L, 
           4L, 3L, 1L, 3L, 2L, 1L, 5L, 2L, 3L, 1L, 4L, 3L, 1L, 3L, 0L, 0L, 
           1L, 0L, 2L, 1L, 2L, 3L, 2L, 3L, 2L, 1L, 2L, 4L, 1L, 0L, 0L, 2L, 
           1L, 5L, 2L, 7L, 1L, 2L, 5L, 3L, 2L, 0L, 0L, 1L, 1L, 1L, 1L, 3L, 
           0L, 1L, 0L, 1L, 1L, 0L, 1L, 2L, 0L, 3L, 2L, 2L, 0L, 3L, 1L, 1L, 
           2L, 1L, 2L, 0L, 2L, 2L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 2L, 0L, 0L, 
           2L, 3L, 0L, 0L, 0L, 0L, 2L, 0L, 3L, 0L, 1L, 0L, 2L, 0L, 0L, 2L, 
           2L, 2L, 1L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 1L)
  # nb 255 bins because 0 is ignored
  expect_equal(counts,tabulate(x,nbins=255))
})

test_that("is.amiramesh and amiratype",{
  amfiles=dir("testdata/amira", pattern = '\\.am$', full.names = T)
  expect_true(all(is.amiramesh(amfiles)))
  # not enough to have a file ending
  tf=tempfile(fileext='.am')
  writeLines("#somethingelse",tf)
  on.exit(unlink(tf))
  expect_false(is.amiramesh(tf))

  expect_equal(amiratype("testdata/amira/AL-a_M.am"), 'uniform.field')
  expect_equal(amiratype("testdata/neuron/testneuron_fclineset.am.gz"),'HxLineSet')
  expect_equal(amiratype("testdata/amira/tetrahedron.surf"), 'HxSurface')
  expect_equal(amiratype("testdata/neuron/Neurites.am"), 'SkeletonGraph')
  expect_equal(amiratype("testdata/amira/landmarks.am"), 'LandmarkSet')
  expect_equal(amiratype("testdata/neuron/EBT7R.CNG.swc"), NA_character_)
  
  expect_true(all(is.amiramesh.im3d("testdata/amira/AL-a_M.am")))
  expect_false(is.amiramesh.im3d("testdata/amira/landmarks.am"))
  expect_false(is.amiramesh.im3d(tf))
})

test_that("getformatreader chooses appropriate class", {
  expect_equal(getformatreader("testdata/neuron/EBT7R.am")$class, "neuron")
  expect_equal(getformatreader("testdata/amira/AL-a_M.am")$class, "im3d")
  expect_equal(getformatreader("testdata/amira/landmarks.am")$class, "landmarks")
})
