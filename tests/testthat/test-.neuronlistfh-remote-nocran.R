context("neuronlistfh remote")

test_that("we can download a neuronlistfh object with MD5'd objects", {
  localdir <- tempfile()
  dir.create(localdir)
  on.exit(unlink(localdir, recursive=TRUE))
  kcs20.url="http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds"
  kcs20md5 <- read.neuronlistfh(kcs20.url, localdir=localdir, quiet=TRUE)
  # test trying to read in neuronlistfh object which is now available locally
  # before we have downloaded any data objects
  kcs20md5.2 <- read.neuronlistfh(kcs20.url, localdir=localdir, quiet=TRUE)
  expect_equal(dim(kcs20md5[[1]]$points), c(284, 3))
  
  # test updating the neuronlistfh object after messing up the current version
  writeLines('Rhubarb crumble!', attr(kcs20md5.2, 'file'))
  expect_error(read.neuronlistfh(kcs20.url, localdir=localdir))
  expect_message(kcs20md5.3<-read.neuronlistfh(kcs20.url, localdir=localdir, 
                                               quiet=TRUE, update=TRUE),
                 "Updating cached")
  expect_equal(kcs20md5.3, kcs20md5.2)
})

test_that("we can synchronise a neuronlistfh object with its remote", {
  localdir <- tempfile()
  dir.create(localdir)
  on.exit(unlink(localdir, recursive=TRUE))
  kcs20fh.remote <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds",
                                      localdir=localdir)
  expect_equal(dim(kcs20fh.remote[[1]]$points), c(284, 3))
  # make a neuronlistfh object from the local data bundled with this package
  # pointing the database directory to the same location as kcs20fh.remote
  kcs20fh.local=as.neuronlistfh(kcs20, dbdir=attr(kcs20fh.remote, 'db')@dir)

  kfm=attr(kcs20fh.remote,'keyfilemap')
  dbdir=attr(kcs20fh.remote, 'db')@dir
  files_before=dir(dbdir)
  # now sync (nothing should happen, only object)
  remotesync(kcs20fh.remote)
  files_after=dir(dbdir)
  expect_equal(files_before,files_after)
  
  # now sync (nothing should happen since there should be no missing files)
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE)
  files_after=dir(dbdir)
  expect_equal(files_before,files_after)
  
  # delete a file and check it is downloaded
  unlink(file.path(dbdir,files_before[1]))
  remotesync(kcs20fh.remote, update.object=FALSE)
  expect_equal(files_before[-1],dir(dbdir))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE)
  expect_equal(files_before,dir(dbdir))
  
  # delete a file and check that we can sync when specifying indices
  unlink(file.path(dbdir,files_before[1]))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE, 
             indices='rhubarb')
  expect_equal(files_before[-1],dir(dbdir))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE, 
             indices=names(kcs20fh.remote))
  expect_equal(files_before,dir(dbdir))
    
  # add an extra file and check it is deleted
  tf=tempfile(tmpdir=dbdir)
  writeLines('rhubarb',con=tf)
  remotesync(kcs20fh.remote, update.object=FALSE, delete.extra=TRUE)
  expect_equal(files_before,dir(dbdir))
  
  # check that we can synchronise when just giving path to object on disk
  expect_equal(remotesync(attr(kcs20fh.remote,'file'),
                          update.object=FALSE,download.missing=TRUE),
               kcs20fh.remote)
})
