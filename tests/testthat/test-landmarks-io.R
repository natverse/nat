context("landmarks io")

test_that("we can read unpaired Amira landmarks",{
  testData=matrix(rnorm(15),ncol=3)
  tmpfile=tempfile()
  write.landmarks.amira(testData,tmpfile)
  testData.new=read.landmarks.amira(tmpfile)
  unlink(tmpfile)
  names(testData.new)<-NULL
  expect_equal(testData,testData.new,tol=1e-6)
})

test_that("we can read/write paired Amira Landmarks",{
  testData=replicate(2,matrix(rnorm(15),ncol=3),simplify=FALSE)
  tmpfile=tempfile()
  
  write.landmarks.amira(testData,tmpfile)
  testData.new=read.landmarks.amira(tmpfile)
  unlink(tmpfile)
  names(testData.new)<-NULL
  expect_equal(testData,testData.new,tol=1e-6)	
})

test_that("we can read Fiji landmarks", {
  baseline=structure(c(256.922344, 200.312336, 441.060392,
                       11.819672, 92.691112, 127.52804, 
                       66.563416, 84.603968, 46.034512),
                     .Dim = c(3L, 3L), .Dimnames = list(
                         c("right_alpha_tip", "right_tract_cross_vLH", 
                           "left_vlpr_tract_crossing"),
                         c("X", "Y", "Z")))
  
  expect_equal(read.landmarks.fiji("testdata/landmarks/JFRC2.points"), baseline,
               tolerance=1e-6)
  
  expect_equal(read.landmarks.fiji("testdata/landmarks/JFRC2_single.points"),
               baseline[1, , drop=FALSE], tolerance=1e-6)
})

test_that("we can identify Fiji landmarks", {
  ff=dir(file.path("testdata", c("amira", "landmarks")), full.names = T)
  expect_equal(length(fjl<-Filter(is.fijilandmarks, ff)), 2L)
  expect_true(all(basename(fjl) %in% c("JFRC2.points", "JFRC2_single.points")))
  expect_equal(getformatreader(fjl[1])$format, "fijilandmarks")
})

test_that("generic landmarks I/O", {
  expect_is(read.landmarks("testdata/amira/landmarks.am"), "landmarks")
  expect_is(l<-read.landmarks("testdata/landmarks//JFRC2.points"), "landmarks")
  write.landmarks(l, tf<-tempfile(fileext = "test.landmarks"), format='cmtk')
  expect_equivalent(read.landmarks(tf), l)
  unlink(tf)
  
  expect_equal(getformatwriter(format='amira', file='test', class='landmarks')$file,
               "test.landmarkAscii")
})
