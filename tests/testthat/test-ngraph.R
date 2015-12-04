context("ngraph class")
library(igraph, warn.conflicts = FALSE)

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
  
  myidentical_graph<-function(target, current, ...){
    old_igraph = package_version(igraph::igraph.version())<'1.0'
    if(old_igraph) isTRUE(all.equal(target, current, ...))
    else igraph::identical_graphs(target, current)
  }
  
  expect_true(myidentical_graph(g1,g1s))
  expect_true(myidentical_graph(g2,g2s))
  expect_true(myidentical_graph(g3,g3s))
})

test_that("we can construct an empty ngraph",{
  expect_is(ngraph(matrix(nrow=0,ncol=2),vertexlabels = 1, xyz=cbind(0,0,0,0)),
            'ngraph')
})

test_that("we can find the length of the spine of a neuron", {
  n <- Cell07PNs[[1]]
  spine.length <- spine(n, LengthOnly = TRUE)
  spine.length.expected <- 186.085903694106
  expect_equal(spine.length, spine.length.expected, tolerance=1e-4)
})

test_that("we can find the path of the spine of a neuron", {
  n <- Cell07PNs[[1]]
  expect_is(spine <- spine(n), 'neuron')
  spine.expected <- readRDS('testdata/neuron/testCell07PNs1_spine.rds')
  expect_equal(spine, spine.expected)
  expect_equal(spine(n, LengthOnly = T), 186.0859, tol=1e-4)
  
  # check that we get the same result when using start point,
  # since that is part of result
  expect_equal(spine(n, UseStartPoint = T), spine.expected)
  expect_equal(spine(n, UseStartPoint = T, LengthOnly = T), 186.0859, tol=1e-4)
  
  # check that can cope with point labels other than 1:n
  n$d$PointNo=n$d$PointNo+1
  spine.expected$PointNo=spine.expected$PointNo+1
  expect_equal(spine, spine.expected)
})

test_that("setting of graph attributes",{
  gatts=list(name='testneuron',nclass="PN")
  expect_is(testg <- as.ngraph(testd, graph.attributes = gatts, 
                               vertex.attributes=list(X=testd$X)), 'ngraph')
  expect_equal(get.graph.attribute(testg, name = 'nclass'), gatts$nclass)
  expect_equal(get.graph.attribute(testg, name = 'name'), gatts$name)
  # null attributes
  expect_equal(get.graph.attribute(testg, name = 'rhubarb'), gatts$rhubarb)
  
  # vertex attributes
  expect_equal(get.vertex.attribute(testg, name = 'X'), testd$X)
  expect_warning(testg <- as.ngraph(testd, graph.attributes = gatts, 
                               vertex.attributes=list(X=testd$X[-1])))
})

test_that("graph weights can be calculated and set",{
  # weights for a neuron with unit length segments
  g1=as.ngraph(testd, weights=TRUE)
  expect_is(g2 <- as.ngraph(testd, weights=FALSE), 'ngraph')
  expect_equal(igraph::E(g1)$weight, rep(1, 5))
  expect_equal(igraph::diameter(g1), igraph::diameter(g2))
  g1=as.ngraph(testd, weights=rep(2,5))
  expect_equal(igraph::E(g1)$weight, rep(2, 5))
  
  g3=as.ngraph(Cell07PNs[[1]], weights=TRUE)
  expect_equal(sum(igraph::E(g3)$weight), 297.1763, tolerance=1e-4)
})

test_that("we can find the segmentgraph of a neuron",{
  testn=as.neuron(testd)
  expect_is(sg<-segmentgraph(testn), 'igraph')
  
  baseline=graph(c(1,2,2,3,2,4), directed=TRUE)
  expect_true(graph.isomorphic(sg, baseline))
  
  expect_equal(E(sg)$weight, c(2, 2, 1))
  expect_true(graph.isomorphic(segmentgraph(testn, weights=FALSE), sg))
  
  # check that we can make a segment graph where each edge direction is
  # reversed
  expect_is(sgr<-segmentgraph(testn, reverse.edges = TRUE), 'igraph')
  expect_equal(ends(sgr,E(sgr)), ends(sg,E(sg))[,2:1])
  
  # and with segment ids included
  expect_is(sgs<-segmentgraph(testn, segids = TRUE), 'igraph')
  expect_equal(E(sgs)$segid, 1:3)
})


test_that("as.ngraph can convert undirected graph into an ngraph object",{
  g1=as.ngraph(testd)
  expect_is(g2<-as.ngraph(as.undirected(g1), root = rootpoints(g1)), 'ngraph')
  expect_true(graph.isomorphic(g1,g2))
})

test_that("Strahler order", {
  n = as.neuron(testd)
  expect_equal(strahler_order(n), list(points = c(2L, 2L, 
    2L, 1L, 1L, 1L), segments = c(2L, 1L, 1L)))
  
  ns=structure(list(NumPoints = 3L, StartPoint = 1L, BranchPoints = integer(0), 
    EndPoints = c(1L, 3L), nTrees = 1, NumSegs = 1L, 
    SegList = structure(list(1:3), class = c("seglist", 
      "list")), d = structure(list(PointNo = 1:3, Label = c(2, 
      2, 2), X = c(1, 2, 3), Y = c(1, 1, 1), Z = c(0, 
      0, 0), W = c(NA, NA, NA), Parent = c(-1L, 1L, 
      2L)), .Names = c("PointNo", "Label", "X", "Y", 
      "Z", "W", "Parent"), class = "data.frame", row.names = c(NA, 
      -3L))), .Names = c("NumPoints", "StartPoint", 
    "BranchPoints", "EndPoints", "nTrees", "NumSegs", 
    "SegList", "d"), class = c("neuron", "list"))
  expect_equal(prune_strahler(n, orderstoprune = 1L), ns)
})

