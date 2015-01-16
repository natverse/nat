context("xformimage")

if(!is.null(cmtk.bindir())){
  test_that("xform(image) can carry out a simple reformat operation", {
    td<-tempfile(pattern = 'xformimage_test')
    dir.create(td)
    on.exit(unlink(td))
    xformt=expression(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                            reg = 'testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list/', 
                            target='testdata/nrrd/FCWB_2um_mask.nrrd',
                            interpolation="nn",
                            out=td, Verbose=F))
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask-JFRC2-444_mask.nrrd")
    # check that we reformatted as expected
    imout=read.im3d(out)
    baseline=structure(c(1904662L, 592730L), .Dim = 2L, .Dimnames = structure(list(
    imout = c("0", "255")), .Names = "imout"), class = "table")
    expect_equal(table(imout), baseline)
    # verify bounding box of output
    bb_out=structure(c(0, 561.9999, 0, 326.0003, 0, 106), .Dim = 2:3, class = "boundingbox")
    expect_equal(boundingbox(imout), bb_out)
  })
  
  test_that("xform(image) can carry out a simple reformat operation with an in-memory registration", {
    td<-tempfile(pattern = 'xformimage_test')
    dir.create(td)
    on.exit(unlink(td))
    xformt=expression(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                            reg = cmtkreglist(cmtkparams2affmat()),
                            target='testdata/nrrd/FCWB_2um_mask.nrrd',
                            interpolation="nn",
                            out=td, Verbose=F))
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask-JFRC2-444_mask.nrrd")
    # check that we reformatted as expected
    imout=read.im3d(out)
    baseline=structure(c(1566120L, 931272L), .Dim = 2L, .Dimnames = structure(list(
      imout = c("0", "255")), .Names = "imout"), class = "table")
    expect_equal(table(imout), baseline)
    # verify bounding box of output
    bb_out=structure(c(0, 561.9999, 0, 326.0003, 0, 106), .Dim = 2:3, class = "boundingbox")
    expect_equal(boundingbox(imout), bb_out)
  })
}
