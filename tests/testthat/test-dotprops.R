context("dotprops objects")

test_that("dotprops gives same result as matlab original", {
  dotdir="testdata/dotprops/masse"
  
  # We have some matlab data from Masse et al 2011 that was read in like this:
  #   dots4=t(readMat(file.path(TestDir,"Geometry","SAKW13-1_dots4.mat"))[[1]])
  #   props4=readMat(file.path(TestDir,"Geometry","SAKW13-1_dots4_properties.mat"))
  # and resaved as rda files
  
  # load in R saves of matlab data
  
  # dots4
  load(file.path(dotdir,'SAKW13-1_dots4.rda'))
  # props4
  load(file.path(dotdir,'SAKW13-1_dots4_properties.rda'))
  # reorder matrices into R forms
  props4$alpha=as.numeric(props4$alpha)
  props4$vect=t(props4$vect)
  
  # make a complete dotprops object out of matlab data
  props4.dotprops=as.dotprops(c(list(points=dots4),props4))
  
  # recalculate dotprops (k=20)
  props4.new<-dotprops(dots4)
  
  expect_equal(props4.new,props4.dotprops,tol=1e-6)
})

test_that("a dotprops object can be made from a nrrd, via im3d", {
  imageFile <- "testdata/nrrd/FruMARCM-F000002_seg001_03-acrop.nrrd"
  expect_is(dp<-dotprops(imageFile), 'dotprops')
  points.expected <- structure(c(188.493801707317, 188.493801707317, 191.402656585366, 
                                 194.893282439024, 189.075572682927, 102.391696778523, 102.391696778523, 
                                 101.228154765101, 101.228154765101, 101.809925771812, 31, 32, 
                                 33, 33, 33), .Dim = c(5L, 3L), .Dimnames = list(NULL, c("X", "Y", "Z")))
  expect_equal(dp$points[1:5, ], points.expected, tol=1e-4)
})

test_that("make a dotprops object from a neuron",{
  expect_is(x<-dotprops(Cell07PNs[[1]], k=5), 'dotprops')
  expect_equal(xyzmatrix(x), xyzmatrix(Cell07PNs[[1]]))
  expect_is(x<-dotprops(Cell07PNs[[1]], resample=1), 'dotprops')
})

test_that("pruning a dotprops object with itself results in no change", {
  kc1=kcs20[[1]]
  expect_equal(prune(kc1, kc1, maxdist=0), kc1)
})

test_that("pruning with different input types behaves", {
  kc1=kcs20[[1]]
  xyz=xyzmatrix(kc1)
  expect_equal(prune(kc1, xyz, maxdist=0), kc1)
  expect_equal(prune(xyz, kc1+1, maxdist=4), xyz)
})

test_that("pruning with different output types behaves", {
  kc1=kcs20[[1]]
  xyz=xyzmatrix(kc1)
  xyzn=xyz+matrix(rnorm(mean = 2, prod(dim(xyz))), ncol=ncol(xyz))
  inds1=prune(xyz, xyzn, maxdist=2, return.indices = TRUE)
  inds2=prune(xyz, xyzn, maxdist=2, keep = 'far', return.indices = TRUE)
  expect_true(all(xor(inds1, inds2)),
              info = "xor of near and far indices includes all points")
})

test_that("We can prune a neuronlist",{
  expect_is(pruned<-prune(kcs20[1:2], kcs20[[2]], maxdist=5), 'neuronlist')
  expect_equal(pruned[[1]], prune(kcs20[[1]], kcs20[[2]], maxdist=5),
               info='pruning a neuronlist gives same result as individual neurons')
  expect_equal(pruned[[2]], prune(kcs20[[2]], kcs20[[2]], maxdist=5),
               info='pruning a neuronlist gives same result as individual neurons')
})

test_that("We can prune a neuronlist with a neuronlist",{
  expect_is(pruned<-prune(kcs20[1:2], kcs20[1:2], maxdist=0), 'neuronlist')
  expect_equal(pruned, kcs20[1:2])
})

context('dotprops arithmetic')
test_that("math operators",{
  kcs13=kcs20[1:3]
  expect_equal(kcs13*-1, -kcs13)
  expect_equal(kcs13/2, kcs13*0.5)
  # ensure that test operates directly via *.dotprops methods rather than
  # *.neuronlist
  expect_equal(kcs13[[1]]/2, kcs13[[1]]*0.5)
  expect_equal(scale(kcs20[[1]], center=T, scale=rep(2,3)), 
               (kcs20[[1]]-colMeans(xyzmatrix(kcs20[[1]])))*0.5)
  
})
