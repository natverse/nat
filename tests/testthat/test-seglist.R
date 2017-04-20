context("seglist methods")

test_that("make a seglist directly",{
  expect_is(seglist(),'seglist')
  expect_is(seglist(list(c(1),c(2,3,4,5))),'seglist')
})

test_that("convert graph to seglist",{
  # simple linear graph
  g=graph(c(1, 2, 2, 3))
  sl=seglist(c(1, 2, 3))
  expect_is(sl,'seglist')
  expect_equal(as.seglist(g), sl)
  
  # invalid input
  expect_error(as.seglist(1:10))
  
  # empty graph
  expect_null(as.seglist(igraph::graph.empty()))
  expect_warning(sl<-as.seglist(igraph::graph.empty(), Verbose = T))
  
  # simple linear graph with different vids
  g=graph(c(1, 2, 2, 3))
  igraph::V(g)$vid=3:5
  sl=seglist(3:5)
  expect_equal(as.seglist(g), sl)
  
  # simple linear graph with different vids and different origin
  g=graph(c(1, 2, 2, 3))
  igraph::V(g)$vid=3:5
  sl=seglist(5:3)
  expect_equal(as.seglist(g, origin=5), sl)
  
  # simple linear graph with different vids and origin at centre, resulting
  # in a branched seglist
  g=graph(c(1, 2, 2, 3))
  igraph::V(g)$vid=3:5
  sl=seglist(4:3,4:5)
  expect_equal(as.seglist(g, origin=4), sl)
  
  # multiple subtrees -> exception since seglist only defined for 1 subtree
  g=graph(c(1,2,2,3,3,4,5,6))
  expect_error(as.seglist(g))
  
  # cyclic graph -> exception since seglist is undefined
  g=graph(c(1, 2, 2, 3, 3, 1))
  expect_error(as.seglist(g))
  
  # single floating point
  g=graph(NULL,n=1)
  expect_equal(as.seglist(g),seglist(1))
  
  # single floating point with different vid
  igraph::V(g)$vid=4
  expect_equal(as.seglist(g),seglist(4))
  
  # trifurcation
  g=graph(c(1,2, 2,3, 2,4, 2,5, 5,6, 6,7))
  sl=seglist(c(1,2),c(2,3),c(2,4),c(2,5,6,7))
  expect_equal(as.seglist(g),sl)
  # undirected equivalent - nb origin must be specified
  expect_equal(as.seglist(as.undirected(g),origin=1),sl)
  
  # rapid branching
  g=graph(c(1,2, 2,3, 2,4, 4,5, 4,6))
  sl=seglist( c(1,2),c(2,3),c(2,4),c(4,5),c(4,6) )
  expect_equal(as.seglist(g),sl)
  
  # different root
  g=graph(c(1,2, 2,3, 2,4, 4,5, 4,6))
  sl=seglist( c(6,4),c(4,2),c(2,1),c(2,3),c(4,5) )
  expect_equal(as.seglist(g,origin=6),sl)
  
  # non-sequential numbering
  g=graph(c(1,2, 2,6, 2,4, 4,5, 4,3))
  sl<-seglist( c(1,2),c(2,4),c(4,3),c(4,5),c(2,6) )
  expect_equal(as.seglist(g,origin=1),sl)
  
  # non-sequential numbering with vertex labels
  # in this case we imagine that there are a set of vertices with PointNo
  # 2,3,4,6,7,9
  g=ngraph(c(2,4,4,3,3,6,6,9,6,7),vertexlabels=c(2:4,6,7,9))
  sl=seglist(c(1,3,2,4),c(4,5),c(4,6))
  expect_equal(as.seglist(g,origin=1),sl)
  # same but with a different origin
  # NB origin is defined in terms of sequential raw vertex id so 
  # origin=3 means that the origin is the vertex with label=4
  sl2=seglist(c(3,1),c(3,2,4),c(4,5),c(4,6))
  expect_equal(as.seglist(g,origin=3),sl2)
})

test_that("seglist2swc",{
  n=Cell07PNs[[1]]
  expect_equivalent(seglist2swc(n)$d, n$d)
  expect_equivalent(seglist2swc(as.seglist(n, all=F), n$d), n$d)
  expect_equivalent(seglist2swc(as.seglist(n, all=T), n$d), n$d)
  
  # edge case length 1 segment in first position
  baseline=data.frame(PointNo=1:6, Label=2L, X=1, Y=1, Z=1, W=NA_real_,
                      Parent=c(-1L,1L,2L,3L,2L,5L))
  expect_equal(seglist2swc(list(1:2, 
                                2:4, 
                                c(2, 5:6)
                                ), 
                           d = xyzmatrix(matrix(1, ncol = 3, nrow = 6))),
               baseline)
  # edge case: linear graph with origin in centre
  sl=list(2:1, 2:5)
  # NB Parent is just linear order i.e. not consistent with origin as defined
  # in seglist - but this should not matter since we will recalculate
  d.input=normalise_swc(data.frame(Parent=c(-1, 1:4)))
  d.baseline=normalise_swc(data.frame(Parent=c(2, -1, 2:4)))
  expect_equal(seglist2swc(sl, d.input), d.baseline)
})
