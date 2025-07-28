skip_if_not_installed('Rvcg')

bl=neuronlist(icosahedron3d(), tetrahedron3d())
names(bl)=c("a","b")
td=tempfile()
setup(dir.create(td))
teardown(unlink(td, recursive = TRUE))

test_that("read/write works", {
  ff1 <- write.neurons(bl, dir=td, format='ply')
  md5.1=tools::md5sum(ff1)
  expect_warning(ff2 <- write.neurons(bl, dir=td, Force=TRUE), regexp = 'ply')
  md5.2=tools::md5sum(ff2)
  expect_equal(md5.1, md5.2)
  
  # error if we try to write one object with write.neurons
  expect_error(write.neurons(bl[[1]], dir=td, format='ply'))
  
  expect_is(bl2 <- read.neurons(td, format='ply'), 'neuronlist')
  expect_equal(sbl <- summary(bl), summary(bl2))
  
  expect_is(sbl, 'data.frame')
  expect_known_value(sbl, file = 'testdata/summary_bl.rds')
  
  expect_error(write.neurons(Cell07PNs[1:3], format = 'ply'))
  
  write.neuron(MBL.surf, file=file.path(td, "MBL.surf.ply"), format = 'ply')
  expect_equal(read.neuron(file.path(td, "MBL.surf.ply")), as.mesh3d(MBL.surf), tolerance = 1e-6)
})

skip_if_not_installed('readobj')

td2=tempfile()
setup(dir.create(td2))
teardown(unlink(td2, recursive = TRUE))

test_that("read/write works with obj files", {
  ff1 <- write.neurons(bl, dir=td2, format='obj')
  expect_is(bl2 <- read.neurons(td2), 'neuronlist')
  expect_equal(summary(bl), summary(bl2))
  expect_error(write.neurons(Cell07PNs[1:3], format = 'obj'))
})
