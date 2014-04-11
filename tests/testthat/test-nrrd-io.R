context("nrrd IO")

test_that("is.nrrd works",{
  tmpdir=tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir,recursive=TRUE))
  
  origlhmaskfile="testdata/nrrd/LHMask.nrrd"
  lhmaskfile=file.path(tmpdir,
                       sub("\\.nrrd$", '.something', basename(origlhmaskfile)))
  file.copy(origlhmaskfile,lhmaskfile)
  
  expect_true(is.nrrd(origlhmaskfile))
  expect_true(is.nrrd(origlhmaskfile, TrustSuffix=TRUE))
  expect_error(is.nrrd(origlhmaskfile, ReturnVersion=TRUE, TrustSuffix=TRUE),
                 label="Check error when asking for nrrd version using suffix")
  expect_equal(is.nrrd(origlhmaskfile,ReturnVersion=TRUE),4)
  
  expect_true(is.nrrd(lhmaskfile))
  expect_false(is.nrrd(lhmaskfile, TrustSuffix=TRUE))
})

test_that("read.nrrd.header works",{
  origlhmaskfile="testdata/nrrd/LHMask.nrrd"
  expect_is(h<-read.nrrd.header(origlhmaskfile),'list')
  
  baseh=structure(list(type = "uint8", encoding = "gzip", endian = "big", 
      dimension = 3, sizes = c(50, 50, 50), `space dimension` = 3, 
      `space directions` = structure(c(1.39999997615814, 0, 0, 
      0, 1.39999997615814, 0, 0, 0, 1.39999997615814), .Dim = c(3L, 
      3L)), `space origin` = c(0, 0, 0), `space units` = c("microns", 
      "microns", "microns")), .Names = c("type", "encoding", "endian", 
  "dimension", "sizes", "space dimension", "space directions", 
  "space origin", "space units"))
  
  expect_equivalent(h, baseh)
})


test_that("read-write.nrrd works",{
  origlhmaskfile="testdata/nrrd/LHMask.nrrd"
  expect_is(d<-read.nrrd(origlhmaskfile),'array')
  expect_true(is.raw(d))
  expect_equal(sum(d!=0), 28669)
  
  tf=tempfile(fileext='.nrrd')
  on.exit(unlink(tf))
  write.nrrd(d,file=tf,dtype='byte')
  d2=read.nrrd(file=tf)
  expect_equal(d, d2, tol=1e-6)
  
  # compare headers
  h=read.nrrd.header(origlhmaskfile)
  h2=read.nrrd.header(tf)
  common_fields=sort(intersect(names(h),names(h2)))
  expect_equal(common_fields, c("dimension", "encoding", "sizes", 
                               "space dimension", "space directions", 
                               "space origin", "type"))
  expect_equal(h[common_fields],h2[common_fields],tol=1e-6)
})
