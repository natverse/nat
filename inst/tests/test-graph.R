context("graph representations of neurons")

testd=data.frame(PointNo=1:6,Label=2,
                 X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                 Parent=c(-1,1:4,3))

test_that("can identify nodes (root,branch,endpoints) from a graph",{
  g1=as.ngraph(testd)  
  expect_equal(rootpoints(g1),1)
  expect_equal(endpoints(g1),c(1,5,6))
  expect_equal(branchpoints(g1),3)
})

test_that("can identify nodes (root,branch,endpoints) from SWC data",{
  expect_equal(rootpoints(testd),1)
  expect_equal(endpoints(testd),c(1,5,6))
  expect_equal(branchpoints(testd),3)
})
