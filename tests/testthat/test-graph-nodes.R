context("neuron/graph node identification")

testd=data.frame(PointNo=1:6,Label=2,
                 X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                 Parent=c(-1,1:4,3))

testn=as.neuron(testd)
test_that("we can identify nodes (root,branch,endpoints) from a graph",{
  g1=as.ngraph(testd)  
  expect_equal(rootpoints(g1),1)
  expect_equal(endpoints(g1),c(1,5,6))
  expect_equal(branchpoints(g1),3)
})

test_that("we can identify nodes (root,branch,endpoints) from SWC data",{
  expect_equal(rootpoints(testd),1)
  expect_equal(endpoints(testd),c(1,5,6))
  expect_equal(branchpoints(testd),3)
})

test_that("we can identify nodes (root,branch,endpoints) from neuron",{
  n=Cell07PNs[['TKC8R']]
  expect_equal(rootpoints(n),2L)
  expect_equal(endpoints(n),c(2L, 286L, 307L, 315L, 323L, 333L, 496L, 548L))
  expect_equal(branchpoints(n),c(226L, 298L, 306L, 463L))
  
  # Hmm these should work, but seems like SWC data block in that neuron is wrong
  # the tracing was originally in Amira format i.e. the SWC block is not the 
  # primary definition of the neuron, but was created post-hoc. The neuron may
  # need to be reparsed with an up to date version of the amiraskel reader or
  # the swc block should be recalculated.
  g=as.ngraph(n$d)
#   expect_equal(rootpoints(g),2L)
#   expect_equal(endpoints(g),c(2L, 286L, 307L, 315L, 323L, 333L, 496L, 548L))
#   expect_equal(branchpoints(g),c(226L, 298L, 306L, 463L))
  
  n2=Cell07PNs[[1]]
  g2=as.ngraph(n2$d)
  expect_equal(rootpoints(n2),rootpoints(g2))
  expect_equal(endpoints(n2),endpoints(g2))
  expect_equal(branchpoints(n2),branchpoints(g2))
  
  # when no subtrees exist
  expect_error(branchpoints(Cell07PNs[[1]], subtrees = 2))
  expect_error(rootpoints(Cell07PNs[[1]], subtrees = 2))
  expect_error(endpoints(Cell07PNs[[1]], subtrees = 2))
})

test_that("we can find nodes in more complicated neurons",{
  expect_error(rootpoints(testn,subtrees=2))
  # now check that we can cope if nTrees is not set
  # (as is sometimes true for old neurons)
  
  testn$nTrees=NULL
  expect_equal(rootpoints(testn),1)
  expect_error(rootpoints(testn,subtrees=2))
  
  # now more complicated neurons - isloated point 
  testd=data.frame(PointNo=1:6,Label=2,
                   X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                   Parent=c(-1,1:4,-1))
  testn.floating=as.neuron(testd)
  expect_equal(rootpoints(testn.floating),1)
  expect_equal(rootpoints(testn.floating, subtrees=1:2, exclude.isolated=FALSE),c(1,6))
  
  testd2=rbind(testd,c(7,2,7,7,0,NA,6))
  testn.2trees=as.neuron(testd2)
  # check that we get two roots when there are indeed 2 roots
  rps=rootpoints(as.ngraph(testn.2trees))
  expect_equal(rps,c(1,6))
})
