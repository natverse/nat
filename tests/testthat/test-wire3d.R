context("Test plotting options for wireframe 3d")

library(alphashape3d)


test_that("we can plot wireframe for triangular meshes in 3D", {
  
  #prepare the mesh..
  kcs20.a=alphashape3d::ashape3d(xyzmatrix(kcs20), alpha = 10)
  kcs20.mesh=as.mesh3d(kcs20.a)
  
  
  options(nat.plotengine='rgl')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh)
  expect_equal(names(wireframes), "triangles")
  
  options(nat.plotengine='plotly')
  nclear3d()
  wireframes <- wire3d(kcs20.mesh)
  expect_type(wireframes, "list")
  
  
})
