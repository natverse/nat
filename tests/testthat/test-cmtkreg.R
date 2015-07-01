context("cmtkreg functions")
require(testthat)

test_that("as.cmtkreg behaves",{
  base=structure("myreg", class = c("cmtkreg", "character"))
  expect_equal(as.cmtkreg('myreg'),base)
})

test_that("is.cmtkreg works ok for objects", {
  reg='missingregistration.list'
  creg=as.cmtkreg('missingregistration.list')
  expect_false(is.cmtkreg(reg))
  expect_true(is.cmtkreg(creg))
})

test_that("(is.)cmtkreg works ok for files", {
  
  expect_true(is.na(cmtkreg(tempfile())), 'non-existent file => NA')
  
  reg2="testdata/cmtk/dofv1.1wshears.list"
  creg2=cmtkreg(reg2)
  expect_true(is.cmtkreg(creg2))
  expect_true(is.cmtkreg(creg2,filecheck='exists'))
  expect_true(is.cmtkreg(creg2,filecheck='magic'))
  # just using this as directory that exists but is not a registration
  expect_false(is.cmtkreg('testdata/cmtk', 'exists'))
  expect_false(is.cmtkreg('testdata/cmtk', 'magic'))
  
  # refer to file not directory
  expect_equal(cmtkreg("testdata/cmtk/dofv1.1wshears.list/registration"), creg2)
  
  reg3=tempfile(fileext='.list')
  on.exit(unlink(reg3,recursive=TRUE),add=TRUE)
  dir.create(reg3)
  expect_error(cmtkreg(reg3))
  
  # write registration file with good name and bad magic
  reg3r=file.path(reg3,'registration')
  writeLines(text='!TYPEDSTREAM\n',con=reg3r)
  # this should work
  creg3<-cmtkreg(reg3)
  expect_true(is.cmtkreg(creg3))
  expect_true(is.cmtkreg(reg3,filecheck='exists'))
  # but magic is bad
  expect_false(is.cmtkreg(reg3,filecheck='magic'))
  # check handling of gzip file
  unlink(reg3r)
  reg3rgz=file.path(reg3,'registration.gz')
  gzf=gzfile(reg3rgz,'w')
  #on.exit(close(gzf),add=TRUE)
  cat('! TYPEDSTREAM\n',file=gzf,useBytes=TRUE)
  close(gzf)
  expect_true(is.cmtkreg(reg3,filecheck='exists'))
  expect_true(is.cmtkreg(reg3,filecheck='magic'))
})
