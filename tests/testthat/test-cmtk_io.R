context("CMTK input/output functions")

test_that("write.cmtk can write an empty CMTK TypedStream file", {
  tf=tempfile('EmptyTypedStream')
  tf2=tempfile('EmptyTypedStream',fileext='.gz')
  tf3=tempfile('EmptyTypedStreamv2.4')
  on.exit(unlink(c(tf,tf2,tf3)))
  write.cmtk(list(),tf)
  expect_equal(readLines(tf),c("! TYPEDSTREAM 1.1", ""))
  write.cmtk(list(),tf2,gzip=TRUE)
  expect_equal(readLines(tf),readLines(tf2))
  
  write.cmtk(list(),tf3,version=2.4)
  expect_equal(readLines(tf3),c("! TYPEDSTREAM 2.4", ""))
})

test_that("read.cmtk and write.cmtk can round-trip a registration file", {
  reg="testdata/cmtk/dofv1.1wshears.list"
  reglist=read.cmtkreg(reg)
  tf=tempfile('dofv1.1wshears_copy',fileext='.list')
  on.exit(unlink(tf,recursive=TRUE))
  write.cmtkreg(reglist,foldername=tf,version=1.1)
  ctf=cmtkreg(tf,returnDir=TRUE)
  # equivalent because we are not interested in the file.info attributes
  # though this also removes version attribute which might be worth a thought
  expect_equivalent(read.cmtkreg(ctf),reglist)
})

test_that("read cmtk warping registration", {
  reg="testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list"
  aff_base=structure(list(xlate = c(40.36793081, -1.083886192, 20.60925626),
                 rotate = c(2.066985174, 0.1277418389, 1.346021698),
                 scale = c(1.10115207, 1.112225643, 1.398342945),
                 shear = c(0, 0, 0),
                 center = c(318.1980197, 158.9434879, 67.49654964)),
            .Names = c("xlate", "rotate", "scale", "shear", "center"))
  r=read.cmtkreg(reg)
  expect_equal(aff_base,r$registration$affine_xform)
})
