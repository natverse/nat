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
  swc='../testdata/neuron/EBT7R.CNG.swc'
  expect_is(n<-read.neuron(swc),'neuron')
  expect_equal(n$NeuronName,'EBT7R.CNG.swc')
})

test_that("We can set the NeuronName field", {
  swc='../testdata/neuron/EBT7R.CNG.swc'
  n<-read.neuron(swc, NeuronName="rhubarb")
  expect_equal(n$NeuronName,'rhubarb')
  # check that we can use a user defined function to define the NeuronName
  nfun=function(x) sub("\\..*","",basename(x))
  n<-read.neuron(swc, NeuronName=nfun)
  expect_equal(n$NeuronName,'EBT7R')
})
