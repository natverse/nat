test_that("neuronlistz works", {
  tf=tempfile(fileext = '.zip')
  on.exit(unlink(tf))
  write.neurons(Cell07PNs[1:5], tf, format='rds')
  expect_true(is.neuronlist(nz <- neuronlistz(tf, df=Cell07PNs[,])))
  expect_equal(nz[1:5], Cell07PNs[1:5])
  
  tf2=tempfile(fileext = '.zip')
  on.exit(unlink(tf2), add = T)
  skip_if_not_installed('qs')
  write.neurons(Cell07PNs[1:5], tf2, format='qs', include.data.frame = T)
  nz <- neuronlistz(tf2)
  expect_equal(nz[1:5], Cell07PNs[1:5])
  
  expect_equal(nvertices(nz), nvertices(Cell07PNs[1:5]))
  expect_equal(nz[1:5], Cell07PNs[1:5])
  expect_equal(nz *1, Cell07PNs[1:5])
  
  nz2 <- neuronlistz(tf2, patt = "EBH.*")
  expect_equal(rownames(nz2[,]), c("EBH11R", "EBH20L", "EBH20R"))
  expect_equal(nz2[[1]], nz2["EBH11R"][[1]])
  expect_warning(nz2["rhubarb"], regexp = "Dropping")
  expect_equal(nz2[,], nz[names(nz2),])
})
