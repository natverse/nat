context("neuronlistfh")

test_that("neuronlistfh behaves like a neuronlist",{
  kcs5=kcs20[1:5]
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20,dir=tf),'neuronlistfh')
  
  # check that ordering is maintained
  expect_equal(names(kcs20fh),names(kcs20))
  expect_equal(kcs20fh[1:5],kcs5)
  
  # check that l/sapply works and produces equivalent results
  expect_equal(lapply(kcs20fh,length),lapply(kcs20,length))
  expect_equal(sapply(kcs20fh,length),sapply(kcs20,length))
  
  # check subset
  expect_equal(subset(kcs20,type=='gamma'),subset(kcs20fh,type=='gamma'))
  
  # mirror points
  kcs20m=mirror(kcs20,mirrorAxisSize=500,transform='flip')
  kcs20fhm=mirror(kcs20fh,mirrorAxisSize=500,transform='flip')
  expect_equal(kcs20m,kcs20fhm)
  
  # arithmetic
  kcs20t=kcs20+1
  kcs20fht=kcs20fh+1
  expect_equal(kcs20t,kcs20fht)
})

test_that("Can load a previously created on disk neuronlistfh representation",{
  # create on disk filehash with one file per neuron
  fhpath=tempfile(pattern='kcs20fh')
  kcs20fh=as.neuronlistfh(kcs20,dir=fhpath,filehash.type='RDS')
  plot3d(subset(kcs20fh,type=='gamma'))
  on.exit(unlink(fhpath,recursive=TRUE))
  
  # now save and reload 
  tf=tempfile()
  on.exit(unlink(tf),add=TRUE)
  saveRDS(kcs20fh,file=tf)
  kcs20fh2=readRDS(tf)
  expect_equal(kcs20fh,kcs20fh2)
  expect_equal(as.neuronlist(kcs20fh),as.neuronlist(kcs20fh2))
})
