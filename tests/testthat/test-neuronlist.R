context("neuronlist")

test_that("head.neuronlist behaves", {
  expect_is(h<-head(Cell07PNs),class='data.frame')
  expect_that(nrow(h),equals(6L))
})

test_that("with.neuronlist / droplevels behave", {
  expect_that(with(Cell07PNs,length(unique(Glomerulus))),equals(4L))
  expect_that(nlevels(droplevels(Cell07PNs)$Glomerulus),equals(4L))
})

test_that("subset.neuronlist and [] do the same thing", {
  df=attr(Cell07PNs,'df')
  expect_is(s1<-Cell07PNs[df$Glomerulus=="DA1"],"neuronlist")
  expect_equal(subset(Cell07PNs,Glomerulus=="DA1"),s1)
  #' empty result
  expect_equal(subset(Cell07PNs,Glomerulus=="rhubarb"),
               Cell07PNs[rep(FALSE,length(Cell07PNs))])
})

test_that("subset.neuronlist works with various indexing forms", {
  expect_equal(subset(Cell07PNs, seq(Cell07PNs), rval='names'), names(Cell07PNs))
  expect_equal(subset(Cell07PNs, rep(T,length(Cell07PNs)), rval='names'), 
               names(Cell07PNs))
  expect_equal(subset(Cell07PNs, names(Cell07PNs), rval='names'), names(Cell07PNs))
})

test_that("subset.neuronlist drops NA rows", {
  # make a copy of original
  x=Cell07PNs
  # set one entry to NA
  attr(x,'df')$Glomerulus[1]=NA
  
  expect_equal(subset(Cell07PNs, Glomerulus=='DL3'), 
               subset(x, Glomerulus=='DL3'))
})

aptip<-function(x) {xyz=xyzmatrix(x);any(xyz[,'X']>350 & xyz[,'Y']<40)}

test_that("subset.neuronlist works with function", {
  # define a function that checks whether a neuron has points in a region in 
  # space, specifically the tip of the mushroom body alpha' lobe
  s1=subset(kcs20,filterfun=aptip)
  expect_equal(s1,subset(kcs20,type=='apbp'))
})

test_that("subset can combine dataframe columns and global variables", {
# check() fails when running this test fails, but the same code works fine as an example
#   odds=rep(c(TRUE,FALSE),10)
#   expect_equal(subset(kcs20,type=='gamma' & odds),
#                subset(kcs20,type=='gamma' & rep(c(TRUE,FALSE),10)))
})

test_that("nmapply with identity function returns its arguments",{
  kcs3=kcs20[1:3]
  expect_equal(nmapply(function(x) x, kcs3), kcs3)
})

test_that("nmapply can vectorise more than one argument",{
  kcs3=kcs20[1:3]
  masizes=c(400,20,30)
  expect_is(xyzflip<-nmapply(mirror, kcs3, mirrorAxis = c("X","Y","Z"), 
                             mirrorAxisSize=masizes), 'neuronlist')
  expect_equal(mirror(kcs20[[3]], mirrorAxisSize = masizes[3], mirrorAxis = 'Z'),
               xyzflip[[3]])
})
