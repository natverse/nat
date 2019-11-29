context("Test plotting options for wireframe 3d")

library(alphashape3d)
#prepare the mesh..
kcs20.a=alphashape3d::ashape3d(xyzmatrix(kcs20), alpha = 10)
kcs20.mesh=as.mesh3d(kcs20.a)

test_that("Wireframe for triangular meshes in 3D - default options", {
 
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh)
  expect_equal(names(wireframes), "triangles")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh)
  expect_type(wireframes, "list")
   
})

test_that("Wireframe for triangular meshes in 3D - options of color and transparency", {
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh,alpha = 0.1, col = 'blue')
  expect_equal(names(wireframes), "triangles")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh,alpha = 0.1, col = 'blue')
  expect_type(wireframes, "list")
  
})

test_that("Wireframe for quad meshes", {
  
  quadmesh = rgl::cube3d()
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh,alpha = 0.5, col = 'green')
  expect_equal(names(wireframes), "quads")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh,alpha = 0.5, col = 'red')
  expect_type(wireframes, "list")
  
})

test_that("Change mesh properties", {
  
  color = "yellow"
  quadmesh = rgl::cube3d(color = color)
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh, plotengine = 'rgl')
  expect_equal(names(wireframes), "quads")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh)
  expect_type(wireframes, "list")
  expect_equal(wireframes$x$attrs[2][[names(wireframes$x$attrs)[1]]]$line$color, color)
  
})

test_that("Check if override properties work", {
  
  color = "yellow"
  quadmesh = rgl::cube3d(color = color,meshColor = "edges")
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh, plotengine = 'rgl',col = 'blue', override = TRUE)
  expect_equal(names(wireframes), "quads")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(quadmesh, col = 'blue', override = TRUE)
  expect_type(wireframes, "list")
  expect_equal(wireframes$x$attrs[2][[names(wireframes$x$attrs)[1]]]$line$color, 'blue')
  
  nclear3d()
  wireframes <- wire3d(quadmesh, col = 'blue', override = FALSE)
  expect_type(wireframes, "list")
  expect_equal(wireframes$x$attrs[2][[names(wireframes$x$attrs)[1]]]$line$color, color)
  
})




test_that("Wireframe for shapelist3d objects", {
  
  shapelist <- rgl::shapelist3d(icosahedron3d(), x = rnorm(10), 
                                y = rnorm(10), z = rnorm(10), 
                                col = 1:5, size = 0.3, plot = FALSE)
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(shapelist,alpha = 0.5, col = 'green')
  expect_equal(names(wireframes), rep("triangles", length = length(shapelist)))
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(shapelist,alpha = 0.5, col = 'red')
  expect_type(wireframes, "list")
  
})

test_that("Add options to existing plot or not", {
  
  color = "yellow"
  quadmesh = rgl::cube3d(color = color,meshColor = "edges")
  
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh)
  wireframes <- wire3d(quadmesh)
  expect_equal(names(wireframes), "quads")
  wireframes <- wire3d(kcs20.mesh, add = TRUE)
  expect_equal(names(wireframes), "triangles")
  
  op <- options(nat.plotengine='plotly')
  on.exit(options(op))
  wireframes <- wire3d(kcs20.mesh)
  expect_type(wireframes, "list")
  wireframes <- wire3d(quadmesh, add = TRUE)
  expect_type(wireframes, "list")
  
})


test_that("Wireframe for non-mesh objects", {
  
  kcs20.mesh = c(1,2,3)
  
  op <- options(nat.plotengine='rgl')
  on.exit(options(op))
  expect_error(wire3d(kcs20.mesh), "No wire3d method defined for objects of class: numeric")
  
  options(nat.plotengine='plotly')
  expect_error(wire3d(kcs20.mesh), "No wire3d method defined for objects of class: numeric")
  
})

