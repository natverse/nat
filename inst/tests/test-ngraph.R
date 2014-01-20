context("ngraph class")
library(igraph)

# make a very simple neuron
# 1 2 3b 4 5
#     6
testd=data.frame(PointNo=1:6,Label=2,
                 X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                 Parent=c(-1,1:4,3))

test_that("as.ngraph can convert swc data into an ngraph object",{
  g1=as.ngraph(testd)
  expect_is(g1,'ngraph')
  
  # check we keep ngraph class when modifying ngraph
  expect_is(set.graph.attribute(g1,'origin',1),'ngraph')
  
  # same neuron with PointNo 2 greater
  testd2=data.frame(PointNo=3:8,Label=2,
                   X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                   Parent=c(-1,2+c(1:4,3)))
  g2=as.ngraph(testd2)
  expect_true(graph.isomorphic(g1,g2))
})


test_that("equivalence of seglist and swc methods for as.ngraph.neuron",{
  testn=as.neuron(testd)
  
  # isomorphic
  # 1 2 3b 5 6
  #     4
  testn2=as.neuron(data.frame(
    PointNo=1:6,Label=2,
    X=c(1:3,3,4,5),Y=c(rep(1,3),2,1,1),Z=0,W=NA,
    Parent=c(-1,1:3,3,5)))
  
  # different by vertex position (isomorphic to 1 by connectivity)
  # 1 2 3b 4
  #     5
  #     6
  testn3=as.neuron(data.frame(
    PointNo=1:6,Label=2,
    X=c(1:4,3,3),Y=c(rep(1,4),2,3),Z=0,W=NA,
    Parent=c(-1,1:3,3,5)))
  
  g1=as.ngraph(testn)
  g2=as.ngraph(testn2)
  g3=as.ngraph(testn3)
  #expect_equal(g2,g3)
  
  g1s=as.ngraph(testn,method='seglist')
  g2s=as.ngraph(testn2,method='seglist')
  g3s=as.ngraph(testn3,method='seglist')
  #expect_equal(g2s,g3s)
  
  expect_true(graph.isomorphic(g1,g2))
  expect_true(graph.isomorphic(g1s,g1))
  expect_true(graph.isomorphic(g2s,g2))
  expect_true(graph.isomorphic(g1,g3))
  expect_true(graph.isomorphic(g1s,g3s))
  
  expect_equal(g1,g1s)
  expect_equal(g2,g2s)
  expect_equal(g3,g3s)
})
