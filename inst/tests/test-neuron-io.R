context("basic input output for neurons")

test_that("We can read neurons in rda or rds format", {
  rda=tempfile(fileext='.rda')
  rds=tempfile(fileext='.rds')
  on.exit(unlink(c(rda,rds)))
  
  n=Cell07PNs[[1]]
  save(n,file=rda)
  saveRDS(n,file=rds)
  expect_equivalent(n,read.neuron(rda))
  expect_equivalent(n,read.neuron(rds))
})

test_that("We can read neurons in swc format", {
  swc=tempfile(fileext='.swc')
  writeLines('# dummy swc format file',con=swc)
  on.exit(unlink(swc))
  expect_message(read.neuron(swc),'not yet implemented')
})
