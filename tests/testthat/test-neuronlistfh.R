context("neuronlistfh")

test_that("neuronlistfh behaves like a neuronlist",{
  kcs5=kcs20[1:5]
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=tf),'neuronlistfh')
  
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
  
  # data.frames
  expect_equal(kcs20fh[1:5,], kcs20[1:5,])
})

test_that("subset neuronlistfh without data.frame",{
  kcs5=kcs20[1:5]
  data.frame(kcs5)=NULL
  tf=tempfile('kcs5fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs5fh<-as.neuronlistfh(kcs5, dbdir=tf),'neuronlistfh')
  expect_is(kcs5again<-subset(kcs5fh, names(kcs5)), 'neuronlist')
  # it was the names that disappeared
  expect_equal(kcs5again, kcs5)
})

test_that("we can load a previously created on disk neuronlistfh representation",{
  # create on disk filehash with one file per neuron
  fhpath=tempfile(pattern='kcs20fh')
  dir.create(file.path(fhpath,'data'),recursive=T)
  kcs20fh=as.neuronlistfh(kcs20, dbdir=file.path(fhpath, 'data'), dbClass='RDS')
  plot3d(subset(kcs20fh,type=='gamma'), soma=T)
  on.exit(unlink(fhpath,recursive=TRUE))
  
  # now save and reload 
  tf=tempfile()
  tf2=tempfile()
  on.exit(unlink(c(tf,tf2)),add=TRUE)
  write.neuronlistfh(kcs20fh,file=tf)
  saveRDS(kcs20fh,file=tf2)
  # ensure that write.neuronlistfh and saveRDS produce identical file
  expect_equivalent(tools::md5sum(tf),tools::md5sum(tf2))
  kcs20fh2=read.neuronlistfh(tf)
  # the only difference between the two objects should be the file attribute
  # added by read.neuronlistfh
  attr(kcs20fh2,'file')=NULL
  expect_equal(kcs20fh,kcs20fh2)
  expect_equal(as.neuronlist(kcs20fh),as.neuronlist(kcs20fh2))
})

test_that("we can create a neuronlistfh with a hashmap",{
  fhpath=tempfile(pattern='kcs20fh')
  on.exit(unlink(fhpath,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=fhpath, hashmap=TRUE),
            'neuronlistfh')
  expect_is(attr(kcs20fh,'hashmap'),'environment')
  
  expect_equal(lapply(kcs20fh,length),lapply(kcs20,length))
  expect_equal(sapply(kcs20fh,length),sapply(kcs20,length))
})

test_that("we can create a neuronlistfh without rewriting objects",{
  # make a neuronlistfh
  fhpath=tempfile(pattern='kcs20fh')
  fhdatapath=file.path(fhpath,'data')
  dir.create(fhdatapath,recursive=TRUE)
  on.exit(unlink(fhpath,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=fhdatapath), 'neuronlistfh')
  
  # then make a new copy after removing one file
  kfm=attr(kcs20fh,'keyfilemap')
  unlink(file.path(fhdatapath,kfm[1]))
  # check we are missing one file
  expect_equal(length(dir(fhdatapath)),length(kfm)-1)
  
  # that we don't replace it when WriteObjects='no'
  expect_is(kcs20fh2<-as.neuronlistfh(kcs20, dbdir=fhdatapath, WriteObjects='no'),
            'neuronlistfh')
  expect_equal(length(dir(fhdatapath)),length(kfm)-1)
  
  expect_is(kcs20fh3<-as.neuronlistfh(kcs20, dbdir=fhdatapath, 
                                      WriteObjects='missing'), 'neuronlistfh')
  # and that we put it back when WriteObjects='missing'
  expect_equal(length(dir(fhdatapath)),length(kfm))
  
  expect_equal(sapply(kcs20fh3,length),sapply(kcs20,length))
})

test_that("read.neurons(nlfh) == as.neuronlist(nlfh)",{
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=tf), 'neuronlistfh')
  
  expect_equal(as.neuronlist(kcs20fh), read.neurons(kcs20fh))
})
