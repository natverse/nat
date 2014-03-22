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

context("neuron plotting")

test_that("can plot neurons in 2D", {
  plottedVertices <- plot(Cell07PNs[[1]])
  plottedVertices.expected <- structure(list(PointNo = c(34L, 48L, 51L, 75L, 78L, 95L, 98L, 99L, 108L, 109L, 115L, 119L, 135L, 143L, 160L, 169L, 1L, 42L, 59L, 62L, 80L, 85L, 96L, 100L, 102L, 112L, 117L, 121L, 134L, 148L, 154L, 165L, 172L, 180L, 1L), X = c(220.98664855957, 228.44807434082, 227.528900146484, 250.583908081055, 250.374450683594, 263.807434082031, 266.267333984375, 266.755706787109, 272.031951904297, 272.292327880859, 274.804351806641, 277.783020019531, 272.748596191406, 278.978912353516, 278.77392578125, 282.149841308594, 186.86604309082, 224.706741333008, 229.634338378906, 226.885482788086, 249.886367797852, 253.009902954102, 264.080108642578, 266.322326660156, 267.545959472656, 271.076171875, 274.612823486328, 277.391998291016, 287.121398925781, 281.795806884766, 276.655090332031, 278.328735351562, 282.306945800781, 289.536407470703, 186.86604309082), Y = c(-100.98698425293, -95.4461364746094, -92.0755767822266, -96.9142990112305, -94.3393630981445, -99.7057647705078, -100.097373962402, -99.1845474243164, -104.97306060791, -104.184257507324, -102.846878051758, -103.363876342773, -105.580902099609, -101.788757324219, -109.390472412109, -110.003799438477, -132.709320068359, -109.863616943359, -92.3063659667969, -90.3632507324219, -93.5907897949219, -90.9490432739258, -98.5338973999023, -98.7324066162109, -98.5985946655273, -102.672882080078, -100.920608520508, -102.191482543945, -101.433647155762, -96.6398696899414, -95.0980911254883, -113.756271362305, -111.93212890625, -111.960144042969, -132.709320068359)), .Names = c("PointNo", "X", "Y"), row.names = c("34", "48", "51", "75", "78", "95", "98", "99", "108", "109", "115", "119", "135", "143", "160", "169", "1", "42", "59", "62", "80", "85", "96", "100", "102", "112", "117", "121", "134", "148", "154", "165", "172", "180", "1.1"), class = "data.frame")
  expect_equal(plottedVertices, plottedVertices.expected)
})

test_that("can plot neurons in 3D", {
  plottedLines <- plot3d(Cell07PNs[[1]])$lines
  expect_more_than(plottedLines, 0)
})

test_that("can plot dotprops in 3D", {
  plottedSegments <- plot3d(kcs20[[1]])$segments
  expect_more_than(plottedSegments, 0)
})
