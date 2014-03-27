context("hxsurf (amira) surfaces")

surf_file="testdata/amira/JFRC2_neuropils_almblh_ascii.surf"
surf=read.hxsurf(surf_file)

test_that("can read hxsurf object", {
  expect_equal(nrow(surf$Vertices),2549L)
  expect_equal(surf$RegionList,names(surf$Regions))
  expect_equal(surf$RegionList,
               c("LH_R", "AL_R", "SLP_R", "AVLP_R", "PVLP_R", "IVLP_R", "PLP_R", 
                 "MB_CA_R", "SCL_R", "GNG", "PRW", "LH_L", "AL_L", "SLP_L", "AVLP_L", 
                 "PVLP_L", "PLP_L", "MB_CA_L", "SCL_L"))
  expect_equal(surf$RegionColourList,
               c("#CC2855", "#FF23DA", "#8BCC29", "#27CCCC", "#6728CC", "#CC2763", 
                 "#00A48D", "#5E5ECC", "#C927CC", "#CC28A7", "#29CC85", "#CC2855", 
                 "#FF23D9", "#8BCC28", "#26CCCC", "#6728CC", "#00A38D", "#5D5ECC", 
                 "#C926CC"))
  open3d()
  plot3d(surf,col='red',alpha=0.2)
  clear3d()
  plot3d(surf,alpha=0.2)
  clear3d()
  plot3d(surf,col=rainbow,alpha=0.2)
  rgl.close()
})

test_that("can save and re-read hxsurf object", {
  on.exit(unlink(surffile))
  surffile <- tempfile()
  write.hxsurf(surf, surffile)
  newsurf <- read.hxsurf(surffile)
  expect_equal(newsurf, surf)
})

test_that("can xform hxsurf object", {
  #' tests both mirror, xform and xyzmatrix methods all in one go
  expect_equal(mirror(mirror(surf,mirrorAxisSize=100),mirrorAxisSize=100),surf)
})

if(!is.null(cmtk.bindir())){
test_that("can xform hxsurf object using registration", {
  reg="testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list"
  xform(surf, reg)
})
}
