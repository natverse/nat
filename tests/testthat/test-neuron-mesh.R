bl=neuronlist(icosahedron3d(), tetrahedron3d())
names(bl)=c("a","b")

test_that("read/write works with ngmesh files", {
  td3=tempfile()
  dir.create(td3)
  on.exit(unlink(td3, recursive = TRUE))

  expect_silent(ff1 <- write.neurons(bl, dir=td3, format='ngmesh'))
  expect_is(bl2 <- read.neurons(td3, format = 'ngmesh'), 'neuronlist')
  expect_equal(summary(bl), summary(bl2))
  expect_error(write.neurons(Cell07PNs[1:3], format = 'ngmesh'))
  
  expect_silent(write.neuron(MBL.surf, file = file.path(td3, 'MBL'), ext=NA, format = 'ngmesh'))
  expect_equal(tolerance = 1e-6,
    read.neuron(file.path(td3, 'MBL'), format='ngmesh'),
    as.mesh3d(MBL.surf))

  raw=readBin(ff1[[1]], what = raw(), n = file.size(ff1[[1]]))
  expect_error(nat:::decode_neuroglancer_mesh(raw[-length(raw)]),
               "neuroglancer legacy mesh")
  expect_is(ngm <- nat:::decode_neuroglancer_mesh(raw, format = 'raw'), 'ngmesh')
  expect_equal(nrow(ngm$v), ncol(bl[[1]]$vb))
})

test_that("we can read a remote ngmesh", {
  u=paste0("https://storage.googleapis.com/aedes_whole-brain/meshes/",
           "ol_meshes_2026_09_29/10000")
  m=try(read.neuron(u, format = 'ngmesh'), silent = TRUE)
  skip_if(inherits(m, 'try-error'), "unable to download remote mesh")
  expect_is(m, 'mesh3d')
  expect_true(ncol(m$it) > 1000)
})


test_that("read/write works", {
  skip_if_not_installed('Rvcg')

  td=tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

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
  # tolerance loosened from testthat default (~1.5e-8) to absorb FP drift in
  # Rvcg::vcgArea on arm64 macOS vs. the x86_64-recorded snapshot
  expect_known_value(sbl, file = 'testdata/summary_bl.rds', tolerance = 1e-6)
  
  expect_error(write.neurons(Cell07PNs[1:3], format = 'ply'))
  
  write.neuron(MBL.surf, file=file.path(td, "MBL.surf.ply"), format = 'ply')
  expect_equal(read.neuron(file.path(td, "MBL.surf.ply")), as.mesh3d(MBL.surf), tolerance = 1e-6)
})


test_that("read/write works with obj files", {
  skip_if_not_installed('readobj')

  td2=tempfile()
  dir.create(td2)
  on.exit(unlink(td2, recursive = TRUE))

  ff1 <- write.neurons(bl, dir=td2, format='obj')
  expect_is(bl2 <- read.neurons(td2), 'neuronlist')
  expect_equal(summary(bl), summary(bl2))
  expect_error(write.neurons(Cell07PNs[1:3], format = 'obj'))
})
