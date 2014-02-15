context("nrrd IO")

test_that('is.nrrd works',{
  tmpdir=tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir,recursive=TRUE))
  
  origlhmaskfile="../testdata/nrrd/LHMask.nrrd"
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
