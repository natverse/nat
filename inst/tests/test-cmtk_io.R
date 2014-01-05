context("test CMTK input/output functions")

test_that("can write.cmtk can write an empty CMTK TypedStream file", {
  tf=tempfile('EmptyTypedStream')
  tf2=tempfile('EmptyTypedStream',fileext='.gz')
  tf3=tempfile('EmptyTypedStreamv2.4')
  on.exit(unlink(c(tf,tf2)))
  write.cmtk(list(),tf)
  expect_equal(readLines(tf),c("! TYPEDSTREAM 1.1", ""))
  write.cmtk(list(),tf2,gzip=TRUE)
  expect_equal(readLines(tf),readLines(tf2))
  
  write.cmtk(list(),tf3,version=2.4)
  expect_equal(readLines(tf3),c("! TYPEDSTREAM 2.4", ""))
})

test_that("can read.cmtk and write.cmtk can round-trip a registration file", {
  reg="../testdata/cmtk/dofv1.1wshears.list"
  reglist=read.cmtkreg(reg)
  tf=tempfile('dofv1.1wshears_copy',fileext='.list')
  on.exit(unlink(tf,recursive=TRUE))
  write.cmtkreg(reglist,foldername=tf)
  ctf=cmtkreg(tf,returnDir=TRUE)
  # equivalent because we are not interested in the file attributes
  # though this also removes version attribute which might be worth a thought
  expect_equivalent(read.cmtkreg(ctf),reglist)
})
