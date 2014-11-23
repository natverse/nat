context("landmarks io")

test_that("we can read unpaired Amira landmarks",{
  testData=matrix(rnorm(15),ncol=3)
  tmpfile=tempfile()
  write.amiralandmarks(testData,tmpfile)
  testData.new=read.amiralandmarks(tmpfile)
  unlink(tmpfile)
  names(testData.new)<-NULL
  expect_equal(testData,testData.new,tol=1e-6)
})

test_that("we can read/write paired Amira Landmarks",{
  testData=replicate(2,matrix(rnorm(15),ncol=3),simplify=FALSE)
  tmpfile=tempfile()
  
  write.amiralandmarks(testData,tmpfile)
  testData.new=read.amiralandmarks(tmpfile)
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
