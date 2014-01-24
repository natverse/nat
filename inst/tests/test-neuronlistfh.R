context("neuronlistfh")

test_that("Subset neuronlistfh -> neuronlist",{
  kcs5=kcs20[1:5]
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20,dbName=tf),'neuronlistfh')
  expect_equal(kcs20fh[names(kcs20)[1:5]],kcs5)
  
  expect_equal(subset(kcs20,type=='gamma'),subset(kcs20fh,type=='gamma'))
})

test_that("Can load a previously created on disk neuronlistfh representation",{
  # create on disk filehash with one file per neuron
  fhpath=tempfile(pattern='kcs20fh')
  kcs20fh=as.neuronlistfh(kcs20,dbName=fhpath,filehash.type='RDS')
  plot3d(subset(kcs20fh,type=='gamma'))
  
  # now save and reload 
  tf=tempfile()
  on.exit(unlink(tf),add=TRUE)
  saveRDS(kcs20fh,file=tf)
  kcs20fh2=readRDS(tf)
  expect_equal(kcs20fh,kcs20fh2)
  expect_equal(as.neuronlist(kcs20fh),as.neuronlist(kcs20fh2))
})
