test_that("read/write works", {
  skip_if_not_installed('Rvcg')
  bl=neuronlist(icosahedron3d(), tetrahedron3d())
  names(bl)=c("a","b")
  td=tempfile()
  dir.create(td)
  expect_silent(ff1 <- write.neurons(bl, dir=td, format='ply'))
  md5.1=tools::md5sum(ff1)
  expect_warning(ff2 <- write.neurons(bl, dir=td, Force=TRUE), regexp = 'ply')
  md5.2=tools::md5sum(ff2)
  expect_equal(md5.1, md5.2)
  
  expect_is(bl2 <- read.neurons(td, format='ply'), 'neuronlist')
  expect_equal(nvertices(bl), nvertices(bl2))
  expect_equal(sapply(bl, Rvcg::nfaces), sapply(bl2, Rvcg::nfaces))
  
  unlink(td, recursive = TRUE)
})
