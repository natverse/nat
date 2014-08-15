context("neuronlist")

test_that("c.neuronlist behaves", {
  expect_equal(c(Cell07PNs), Cell07PNs)
  
  expect_equivalent(c(Cell07PNs[1:5], Cell07PNs[6:10]), Cell07PNs[1:10])
  c610.nodf=Cell07PNs[6:10]
  attr(c610.nodf,'df')=NULL
  expect_equivalent(c(Cell07PNs[1:5], c610.nodf), Cell07PNs[1:10])
  expect_error(c(Cell07PNs[1:5], NULL))
  expect_error(c(Cell07PNs[1:5], Cell07PNs[1:5]), "neurons with the same name")
})

test_that("head.neuronlist behaves", {
  expect_is(h<-head(Cell07PNs),class='data.frame')
  expect_that(nrow(h),equals(6L))
})

test_that("with.neuronlist / droplevels behave", {
  expect_that(with(Cell07PNs,length(unique(Glomerulus))),equals(4L))
  expect_that(nlevels(droplevels(Cell07PNs)$Glomerulus),equals(4L))
})

context("neuronlist: subset")

test_that("subset.neuronlist and [] do the same thing", {
  df=attr(Cell07PNs,'df')
  expect_is(s1<-Cell07PNs[df$Glomerulus=="DA1"],"neuronlist")
  expect_equal(subset(Cell07PNs,Glomerulus=="DA1"),s1)
  #' empty result
  expect_equal(subset(Cell07PNs,Glomerulus=="rhubarb"),
               Cell07PNs[rep(FALSE,length(Cell07PNs))])
  # numeric indices
  expect_equal(subset(Cell07PNs, c(1, 3)), Cell07PNs[c(1, 3)])
})

test_that("subset.neuronlist works with various indexing forms", {
  expect_equal(subset(Cell07PNs, seq(Cell07PNs), rval='names'), names(Cell07PNs))
  expect_equal(subset(Cell07PNs, rep(T,length(Cell07PNs)), rval='names'), 
               names(Cell07PNs))
  expect_equal(subset(Cell07PNs, names(Cell07PNs), rval='names'), names(Cell07PNs))
  
  expect_equal(subset(Cell07PNs, 1:2, rval='names'), names(Cell07PNs)[1:2])
  logindices=c(T,T,rep(F,length(Cell07PNs)-2))
  expect_equal(subset(Cell07PNs, c(logindices, NA), rval='names'), names(Cell07PNs)[1:2])
  expect_equal(subset(Cell07PNs, c(1:2, NA), rval='names'), names(Cell07PNs)[1:2])
  expect_equal(subset(Cell07PNs, names(Cell07PNs)[1:3], rval='names'), names(Cell07PNs)[1:3])
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

context("neuronlist: nlapply/nmapply")

test_that("nlapply can omit failures",{
  kcs3=kcs20[1:3]
  kcs3[[3]]=subset(kcs3[[3]],1:4)
  expect_error(dotprops(kcs3, k=5))
  expect_is(dotprops(kcs3, k=5, OmitFailures=FALSE)[[3]], 'try-error')
  expect_is(kcs3.dps<-dotprops(kcs3, k=5, OmitFailures=TRUE), 'neuronlist')
  expect_equal(length(kcs3.dps),2L)
  expect_equal(nrow(attr(kcs3.dps,'df')),2)
  
  # this time with subset and omit failures
  expect_equal(length(dotprops(kcs3, k=5, subset=1:2, OmitFailures=TRUE)), 3)
  expect_equal(length(dotprops(kcs3, k=5, subset=c(1,3), OmitFailures=TRUE)), 2)
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

test_that("nmapply can omit failures",{
  kcs3=kcs20[1:3]
  
  expect_error(nmapply(mirror, kcs20[1:3], mirrorAxis = c("X","Y","Z"),
                  mirrorAxisSize=c(400,20,Inf)))
  expect_equal(length(nmapply(mirror, kcs20[1:3], mirrorAxis = c("X","Y","Z"),
                       mirrorAxisSize=c(400,20,Inf), OmitFailures=TRUE)), 2)
})

context("neuronlist: plot3d")

test_that("plot neuronlist contents",{
  nplotted1 <- length(plot3d(c("EBH11R", "EBH20L"), db=Cell07PNs))
  op=options(nat.default.neuronlist="Cell07PNs")
  expect_equal(length(plot3d(c("EBH11R", "EBH20L"))), nplotted1)
  options(op)
})

test_that("plot3d.neuronlist can work with pre-substituted colour expressions",{
  f=function(...) {
    rhubarb='pink'
    plot3d("EBH20L", col=substitute(rhubarb), db=Cell07PNs, ...)
  }
  expect_error(f())
  expect_is(f(SUBSTITUTE = FALSE), 'list')
})
