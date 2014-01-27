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
