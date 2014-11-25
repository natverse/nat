context("xformimage")


if(!is.null(cmtk.bindir())){
  test_that("xform(image) can carry out a simple reformat operation", {
    td<-tempfile(pattern = 'xformimage_test')
    dir.create(td)
    on.exit(unlink(td))
    xformt=expression(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                                     reg = 'testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list/', 
                                     target='testdata/nrrd/FCWB_2um_mask.nrrd',
                                     out=td, Verbose=FALSE))
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask-JFRC2-444_mask.nrrd")
    
  })
}
