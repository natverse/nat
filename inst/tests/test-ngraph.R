context("ngraph class")
library(igraph)

test_that("as.ngraph can convert swc data into an ngraph object",{
  # make a very simple neuron
  # 1 2 3b 4 5
  #     6
  testd=data.frame(PointNo=1:6,Label=2,
                   X=c(1:5,3),Y=c(rep(1,5),2),Z=0,W=NA,
                   Parent=c(-1,1:4,3))
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
