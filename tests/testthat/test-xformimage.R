context("xformimage")

quicktable<-function(x) {
  xname=deparse(substitute(x))
  tt=tabulate(x+1)
  levels=seq.int(from=0, length.out = length(tt))
  nz=tt!=0L
  
  structure(tt[nz], .Dim = sum(nz), 
            .Dimnames = structure(list(as.character(levels[nz])), .Names = xname),
            class = "table")
}

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
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask_JFRC2-444_mask.nrrd")
    # check that we reformatted as expected
    imout=read.im3d(out)
    baseline=structure(c(1904662L, 592730L), .Dim = 2L, .Dimnames = structure(list(
      imout = c("0", "255")), .Names = "imout"), class = "table")
    expect_equal(quicktable(imout), baseline)
    # verify bounding box of output
    bb_out=structure(c(0, 561.9999, 0, 326.0003, 0, 106), .Dim = 2:3, class = "boundingbox")
    expect_equal(boundingbox(imout), bb_out)
    expect_output(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                       reg = 'testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list/',
                       target='testdata/nrrd/FCWB_2um_mask.nrrd',
                       out=td), "already exists.*use OverWrite")
    
    expect_output(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                        reg = 'testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list/',
                        target='testdata/nrrd/FCWB_2um_mask.nrrd',
                        out=td, OverWrite='update'),
                  "Skipping.*because input files are older")
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
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask_JFRC2-444_mask.nrrd")
    # check that we reformatted as expected
    imout=read.im3d(out)
    baseline=structure(c(1566120L, 931272L), .Dim = 2L, .Dimnames = structure(list(
      imout = c("0", "255")), .Names = "imout"), class = "table")
    expect_equal(quicktable(imout), baseline)
    # verify bounding box of output
    bb_out=structure(c(0, 561.9999, 0, 326.0003, 0, 106), .Dim = 2:3, class = "boundingbox")
    expect_equal(boundingbox(imout), bb_out)
    md5.1=tools::md5sum(file=out)
    
    ident.mat=cmtkparams2affmat()
    xformt=expression(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                            reg = ident.mat,
                            target='testdata/nrrd/FCWB_2um_mask.nrrd',
                            interpolation="nn",
                            out=td, Verbose=F))
    expect_equal(basename(out<-eval(xformt)), "FCWB_2um_mask_JFRC2-444_mask.nrrd")
    # check output files are identical
    expect_equal(tools::md5sum(file=out), md5.1)
    
    # error tests
    expect_error(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                       reg = list("bogusregistration"),
                       target='testdata/nrrd/FCWB_2um_mask.nrrd',
                       interpolation="nn",
                       out=td, Verbose=F),
                 "Unrecognised.* type")
    
    expect_error(xform("testdata/nrrd/JFRC2-444_mask.nrrd", 
                       reg = "testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list",
                       transformtype = "affine",
                       target='testdata/nrrd/FCWB_2um_mask.nrrd',
                       interpolation="nn",
                       out=td, Verbose=F),
                 "Not yet implemented")
    
  })
}
