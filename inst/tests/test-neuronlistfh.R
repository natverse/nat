context("neuronlistfh")

test_that("Subset neuronlistfh -> neuronlist",{
  kcs5=kcs20[1:5]
  expect_is(kcs20fh<-as.neuronlistfh(kcs20),'neuronlistfh')
  expect_equal(kcs20fh[names(kcs20)[1:5]],kcs5)
  
  expect_equal(subset(kcs20,type=='gamma'),subset(kcs20fh,type=='gamma'))
})