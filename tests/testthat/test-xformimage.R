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
    
    # verify bounding box of output
    bb_out=structure(c(0, 561.9999, 0, 326.0003, 0, 106), .Dim = 2:3, class = "boundingbox")
    expect_equal(boundingbox(read.im3d(out, ReadData = F)), bb_out)
  })
}
