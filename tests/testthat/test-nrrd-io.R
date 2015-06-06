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
  
  expect_false(is.nrrd(charToRaw("NRRD00")))
  expect_true(is.nrrd(charToRaw("NRRD0007")))
  expect_false(is.nrrd(bytes=charToRaw("NRRD00")))
  expect_true(is.nrrd(bytes=charToRaw("NRRD0007\n# A comment")))
  expect_error(is.nrrd(f=LETTERS,bytes=charToRaw("NRRD0007")))
  
  # it's more efficient to specify the class, but should still work without
  expect_equal(getformatreader(origlhmaskfile), 
               getformatreader(origlhmaskfile, class='im3d'))
})

test_that("read.nrrd.header works",{
  origlhmaskfile="testdata/nrrd/LHMask.nrrd"
  expect_is(h<-read.nrrd.header(origlhmaskfile),'list')
  
  baseh=list(type = "uint8", encoding = "gzip", dimension = 3, 
             sizes = c(50, 50, 50), 
             `space dimension` = 3, 
             `space directions` = diag(1.39999997615814, 3),
             `space origin` = c(0, 0, 0), 
             `space units` = c("microns", "microns", "microns"))
  
  expect_equivalent(h, baseh)
})

test_that("read-write.nrrd works",{
  origlhmaskfile="testdata/nrrd/LHMask.nrrd"
  expect_is(d<-read.nrrd(origlhmaskfile),'array')
  expect_true(is.raw(d))
  expect_equal(sum(d!=0), 28669)
  
  dir.create(td <- tempfile())
  on.exit(unlink(td, recursive = TRUE))
  tf=tempfile(tmpdir = td, fileext='.nrrd')
  write.nrrd(d,file=tf,dtype='byte')
  d2=read.nrrd(file=tf)
  expect_equivalent(d, d2)
  # ... and as detached nrrd
  tf2=tempfile(tmpdir = td, fileext = '.nhdr')
  write.nrrd(d,file=tf2, dtype='byte')
  d3=read.nrrd(file=tf2)
  expect_equivalent(d, d3)
  
  # compare headers
  h=attr(d,'header')
  h2=attr(d2,'header')
  h3=attr(d2,'header')
  common_fields=sort(intersect(names(h),names(h2)))
  expect_equal(common_fields, c("dimension", "encoding", "sizes", 
                               "space dimension", "space directions", 
                               "space origin", "space units", "type"))
  expect_equal(h[common_fields],h2[common_fields],tol=1e-6)
  expect_equal(h[common_fields],h3[common_fields],tol=1e-6)
  
  # another example with a 1d array
  set.seed(42)
  testhist=hist(rnorm(1000), breaks = 10, plot = F)
  th=tempfile(pattern = 'testhist.nrrd')
  write.nrrd(testhist$counts, file = th, enc = 'text', 
             header=list(axismins=testhist$breaks[1], axismaxs=max(testhist$breaks)))
  
  # TODO this function could actually be useful somewhere ...
  read.nrrd.histogram<-function(f) {
    histdata=read.nrrd(f)
    h=attr(histdata,'header')
    breaks=seq(from=h$axismins, to=h$axismaxs, length.out = h$sizes+1)
    # nb usef of c to remove attributes
    counts=c(histdata)
    structure(list(counts=counts, breaks=breaks,
                   mids = 0.5 * (breaks[-1L] + breaks[-(length(breaks))]),
                   density = counts/(sum(counts) * diff(breaks))),
              class='histogram')
  }
  inhist=read.nrrd.histogram(th)
  expect_equal(unclass(inhist), testhist[names(inhist)])
})

test_that("read/write bad nrrds", {
  expect_error(read.nrrd("testdata/nrrd/badtype.nhdr"), 'data type')
  expect_error(read.nrrd("testdata/nrrd/badenc.nhdr"), 'encoding')
  expect_error(write.nrrd(list('rhubarb'), tempfile()), 'nrrd only accepts')
})

context("detached nrrd data files")

test_that("nrrd.datafiles", {
  # an internal function, but somewhat complex
  expect_error(nrrd.datafiles('testdata/amira/AL-a_M.am'), 'not a nrrd')
  expect_equal(nrrd.datafiles("testdata/nrrd/LHMask.nhdr"), 
               "testdata/nrrd/LHMask.nrrd")

  # check nhdrs for a list of files
  nhdrs=dir("testdata/nrrd", pattern = '\\.nhdr$', full.names = TRUE)
  expect_is(nhdrl<-nrrd.datafiles(nhdrs, full.names = FALSE), 'list')
  
  # check that we can get datafiles for LIST
  expect_equal(nhdrl[['testdata/nrrd/datafile_list.nhdr']], 
               paste0('slice',0:49,'.raw'))
  
  # check that we can get datafiles for LIST with subdim
  # i.e. slabs of data
  slabs=structure(c("slab0-9.raw", "slab10-19.raw", "slab20-29.raw", 
                    "slab30-39.raw", "slab40-49.raw"), subdim = 3L)
  expect_equal(nhdrl[['testdata/nrrd/datafile_listslab.nhdr']], slabs)
})

context("detached nhdr")
test_that("write.nrrd.header.for.file", {
  expect_error(write.nrrd.header.for.file('testdata/amira/AL-a_M.am'),
               "only raw format")
  expect_error(write.nrrd.header.for.file('testdata/amira/landmarks.am'),
               "read.im3d")
  dir.create(td<-tempfile())
  on.exit(unlink(td, recursive = TRUE))
  file.copy('testdata/amira/VerySmallLabelField.am', 
            amcopy <- file.path(td, "VerySmallLabelField.am"))
  expect_is(nhdr<-write.nrrd.header.for.file(amcopy),
               "character")
  expect_equal(read.im3d(nhdr), read.im3d(amcopy))
  
  file.copy('testdata/nrrd/LHMask.nrrd', 
            nrrdcopy <- file.path(td, "LHMask.nrrd"))
  expect_is(nhdr2<-write.nrrd.header.for.file(nrrdcopy),
            "character")
  expect_equal(read.im3d(nhdr2), read.im3d(nrrdcopy))
})
