context("Test plotting options for wireframe 3d")

library(alphashape3d)
#prepare the mesh..
kcs20.a=alphashape3d::ashape3d(xyzmatrix(kcs20), alpha = 10)
kcs20.mesh=as.mesh3d(kcs20.a)

test_that("Wireframe for triangular meshes in 3D - default options", {
 
  options(nat.plotengine='rgl')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh)
  expect_equal(names(wireframes), "triangles")
  
  options(nat.plotengine='plotly')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh)
  expect_type(wireframes, "list")
   
})

test_that("Wireframe for triangular meshes in 3D - options of color and transparency", {
  
  options(nat.plotengine='rgl')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh,alpha = 0.1, col = 'blue')
  expect_equal(names(wireframes), "triangles")
  
  options(nat.plotengine='plotly')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh,alpha = 0.1, col = 'blue')
  expect_type(wireframes, "list")
  
})


test_that("Wireframe for non-mesh objects", {
  
  kcs20.mesh = c(1,2,3)
  
  options(nat.plotengine='rgl')
  nclear3d()
  expect_error(wire3d(kcs20.mesh), "The object supplied is not of mesh3d class")
  
  options(nat.plotengine='plotly')
  nclear3d()
  expect_error(wire3d(kcs20.mesh), "The object supplied is not of mesh3d class")
  
})

