context("hxsurf (amira) surfaces")

surf_file="testdata/amira/JFRC2_neuropils_almblh_ascii.surf"
surf=read.hxsurf(surf_file, RegionChoice = 'Inner')

test_that("we can read hxsurf object", {
  expect_equal(nrow(surf$Vertices),2549L)
  expect_equal(surf$RegionList,names(surf$Regions))
  expect_equal(surf$RegionList,regions<-
               c("LH_R", "AL_R", "SLP_R", "AVLP_R", "PVLP_R", "IVLP_R", "PLP_R", 
                 "MB_CA_R", "SCL_R", "GNG", "PRW", "LH_L", "AL_L", "SLP_L", "AVLP_L", 
                 "PVLP_L", "PLP_L", "MB_CA_L", "SCL_L"))
  expect_equal(surf$RegionColourList,cols<-
               c("#CC2855", "#FF23DA", "#8BCC29", "#27CCCC", "#6728CC", "#CC2763", 
                 "#00A48D", "#5E5ECC", "#C927CC", "#CC28A7", "#29CC85", "#CC2855", 
                 "#FF23D9", "#8BCC28", "#26CCCC", "#6728CC", "#00A38D", "#5D5ECC", 
                 "#C926CC"))
  m=data.frame(name = regions, id = 1:19, col = cols, row.names=regions, 
               stringsAsFactors = FALSE)
  expect_equal(materials(surf), m)
  open3d()
  plot3d(surf,col='red',alpha=0.2)
  clear3d()
  plot3d(surf,alpha=0.2)
  clear3d()
  plot3d(surf,col=rainbow,alpha=0.2)
  rgl.close()
})

test_that("we fail for bad surface files", {
  expect_error(read.hxsurf("testdata/amira/tetrahedron_notriangles.surf"), 
               "Incorrect number of Triangle")
  expect_error(read.hxsurf("testdata/amira/tetrahedron_badtrianglenum.surf"), 
               "Bad triangle")
  expect_error(read.hxsurf("testdata/amira/VerySmallLabelField.am"), 
               "does not appear to be an Amira HyperSurface")
})

test_that("we can use fallback colour for surfaces", {
  tet.hxsurf=read.hxsurf("testdata/amira/tetrahedron.surf")
  tet.hxsurf2=read.hxsurf("testdata/amira/tetrahedron_nocol.surf",
                          FallbackRegionCol = '#FF0000')
  expect_equal(tet.hxsurf2, tet.hxsurf)
})

test_that("we can identify reader/writer for hxsurf", {
  expect_equal(getformatreader(surf_file)$class, 'hxsurf')
  expect_equal(getformatwriter(class=class(surf))$class, 'hxsurf')
})

test_that("we can subset hxsurf object",{
  expect_is(lhr<-subset(surf,"LH_R"),'hxsurf')
  expect_equal(subset(surf, surf$RegionList), surf)
  
  expect_equal(subset(surf, drop=TRUE), surf)
  expect_is(lhr.drop<-subset(surf,"LH_R", drop=TRUE), class = 'hxsurf')
  expect_equal(subset(surf,"^LH"), subset(surf, c("LH_R",'LH_L')))
  expect_equal(subset(surf,"^LH",rval = 'names'), c("LH_R",'LH_L'))
  expect_equal(subset(surf,rval = 'names'), surf$RegionList)
  expect_error(subset(surf,"rhubarb"))
  expect_error(subset(surf,c("rhubarb","and","LH_R")))
  
  simple_surf=structure(list(Vertices = data.frame(X = 10, Y = 10, Z = 1, PointNo= 1:3), 
                             Regions = structure(list(LH_L = data.frame(V1=1,V2=2,V3=3),
                                                      LH_R = data.frame(V1=1,V2=2,V3=3))),
                             RegionList = c("LH_L", "LH_R"),
                             RegionColourList = c("red",'green')), 
                        .Names = c("Vertices", "Regions", "RegionList", "RegionColourList"), 
                        class = c("hxsurf", "list"))
  simple_surf.subset=structure(list(Vertices = data.frame(X = 10, Y = 10, Z = 1, PointNo= 1:3), 
                                    Regions = structure(list(LH_L = data.frame(V1=1,V2=2,V3=3))),
                                    RegionList = c("LH_L"),
                                    RegionColourList = c("red")),
                               .Names = c("Vertices", "Regions", "RegionList", "RegionColourList"), 
                               class = c("hxsurf", "list"))
  
  expect_equal(subset(simple_surf,"LH_L"), simple_surf.subset)
  
  simple_surf_4=simple_surf
  simple_surf_4$Vertices=rbind(simple_surf_4$Vertices, c(12, 12, 2, 4))
  expect_equal(subset(simple_surf_4, "LH_L", drop=TRUE), simple_surf.subset)
})

test_that("we can convert hxsurf to rgl::mesh3d",{
  tet.hxsurf=read.hxsurf("testdata/amira/tetrahedron.surf")
  expect_is(tet.hxsurf,'hxsurf')
  expect_is(tet.mesh3d<-as.mesh3d(tet.hxsurf), 'mesh3d')
  expect_equal(tet.mesh3d, tetrahedron3d(color='#FF0000'))
  
  expect_equal(as.mesh3d(surf, Regions=c("LH_L","LH_R")),
               as.mesh3d(subset(surf, c("LH_L","LH_R"), drop=TRUE)))  
})

test_that("we can save and re-read hxsurf object", {
  on.exit(unlink(surffile))
  surffile <- tempfile()
  write.hxsurf(surf, surffile)
  newsurf <- read.hxsurf(surffile)
  expect_equal(newsurf, surf)
})

test_that("we can xform hxsurf object", {
  #' tests both mirror, xform and xyzmatrix methods all in one go
  expect_equal(mirror(mirror(surf,mirrorAxisSize=100),mirrorAxisSize=100),surf)
})

if(!is.null(cmtk.bindir())){
test_that("we can xform hxsurf object using registration", {
  reg="testdata/cmtk/FCWB_JFRC2_01_warp_level-01.list"
  expect_is(xform(surf, reg), 'hxsurf')
})
}

if(require('Rvcg')){
  test_that('check if points are inside a surface',{
    # the surface and neuron are registered against different templates
    # so cheat by using an approximate offset
    n=kcs20[[1]]+c(40,-30,20)
    MB_CA_L=readRDS("testdata/amira/JFRC2_MB_CA_L.rds")
    expect_equal(sum(pointsinside(n, MB_CA_L)), 55L)
    surf2=read.hxsurf(surf_file, RegionChoice="both")
    MB_CA_L=subset(surf2, "MB_CA_L")
    expect_equal(sum(pointsinside(n, MB_CA_L)), 55L)
  })
}
