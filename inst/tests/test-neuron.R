context("test core neuron methods")

test_that("Can make a neuron",{
  x=Cell07PNs[[1]]
  x2=do.call('neuron',x)
  expect_equal(x,x2)
})

testd=data.frame(PointNo=1:6,Label=2,
                 X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                 Parent=c(-1,1:4,3))

testn=as.neuron(testd)

test_that("as.neuron.ngraph",{
  g<-as.ngraph(testn)
  cn=as.neuron(g,vertexData=testd,origin=1)
  expect_equal(testn$SegList,cn$SegList)
  expect_equal(testd,cn$d)
  
  # vertex labels with gaps
  g=ngraph(c(2,4,4,3,3,6,6,9,6,7),vertexlabels=c(2:4,6,7,9))
  sl=seglist(c(1,3,2,4),c(4,5),c(4,6))
  expect_equal(as.seglist(g,origin=1),sl)
  expect_equal(as.neuron(g,origin=2)$SegList,sl)
  # same but no origin specified (should give same result)
  expect_equal(as.neuron(g)$SegList,sl)
  
  # same but different origin
  sl2=seglist(c(3,1),c(3,2,4),c(4,5),c(4,6))
  expect_equal(as.neuron(g,origin=4)$SegList,sl2)
  
  # same connectivity but one extra (floating) point at end
  g=ngraph(c(2,4,4,3,3,6,6,9,6,7),vertexlabels=c(2:4,6,7,9,10))
  n=as.neuron(g,origin=4)
  expect_equal(n$SegList,sl2)
  expect_equal(n$nTrees,2)
  expect_equal(n$SubTrees,list(sl2,seglist(7)))
  
  # same connectivity but with extra (floating) points at start and end
  g=ngraph(c(2,4,4,3,3,6,6,9,6,7),vertexlabels=c(1:4,6,7,9,10))
  # this will shift all vertex ids by 1
  sl3=as.seglist(lapply(sl2,'+',1))
  n=as.neuron(g,origin=4)
  expect_equal(n$SegList,sl3)
  expect_equal(n$nTrees,3)
  expect_equal(n$SubTrees,list(sl3,seglist(1),seglist(8)))
  
  # 3 separate subgraphs of length 3,4,5
  g=ngraph(c(0,1,1,2, 3,4,4,5,5,6, 7,8,8,9,9,10,10,11),vertexlabels=0:11)
  n=as.neuron(g,origin=0)
  expect_equal(n$SegList,seglist(c(1,2,3)))
  n2=as.neuron(g,origin=3)
  expect_equal(n2$SegList,seglist(c(4,5,6,7)))
  n3=as.neuron(g,origin=7)
  expect_equal(n3$SegList,seglist(c(8,9,10,11,12)))
  # check that it picks largest subgraph when no origin specified
  n4=as.neuron(g)
  expect_equal(n4,n3)
})