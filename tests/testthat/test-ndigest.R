context("ndigest")
library(digest)

test_that("ndigest works for overloaded and regular classes",{
  
  expect_false(isTRUE(all.equal(ndigest(kcs20[[1]]),digest(kcs20[[1]]))))
  expect_equal(ndigest(kcs20[[1]]),"4c045b0343938259cd9986494fc1c2b0")
  
  expect_equal(ndigest(''),digest(''))
  
  
  expect_equal(ndigest(read.neuron('testdata/neuron/EBT7R.am')),
               "f24c1252d17b6bd9898c7842f1ad9f5d")
  
  # three neuronlists with different names but same contents
  tf=tempfile('kcs20fh')
  tf2=tempfile('kcs20fh')
  dir.create(tf)
  dir.create(tf2)
  on.exit(unlink(c(tf,tf2),recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=file.path(tf,'data')),'neuronlistfh')
  expect_is(kcs20fh2<-as.neuronlistfh(kcs20, dbdir=file.path(tf2,'data')),'neuronlistfh')
  expect_equal(ndigest(kcs20fh), ndigest(kcs20fh2))
  write.neuronlistfh(kcs20fh,file=file.path(tf,'kcs20fh.rds'))
  write.neuronlistfh(kcs20fh,file=file.path(tf2,'kcs20fh2.rds'))
  expect_equal(ndigest(kcs20fh), "fb6338dfd6a5adea73bae4cf4efff1a8")
  
  kcs20fh3=read.neuronlistfh(file.path(tf,'kcs20fh.rds'))
  kcs20fh4=read.neuronlistfh(file.path(tf2,'kcs20fh2.rds'))
  expect_equal(ndigest(kcs20fh3), ndigest(kcs20fh))
  expect_equal(ndigest(kcs20fh3), ndigest(kcs20fh4))
})