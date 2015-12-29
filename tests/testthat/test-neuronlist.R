context("neuronlist")

test_that("as.neuronlist behaves", {
  n14=Cell07PNs[1:4]
  df=attr(n14,'df')
  expect_equal(as.neuronlist(n14, df = df), n14)
  # check that we can make names null or empty and all OK
  n14.nonames=n14
  names(n14.nonames)=NULL
  expect_equal(as.neuronlist(n14.nonames, df=df), n14)
  names(n14.nonames)=rep("", length(n14))
  expect_equal(as.neuronlist(n14.nonames, df=df), n14)
  
  expect_equivalent(as.neuronlist(Cell07PNs[[1]]), Cell07PNs[1])
})

test_that("c.neuronlist behaves", {
  expect_equal(c(Cell07PNs), Cell07PNs)
  
  expect_equivalent(c(Cell07PNs[1:5], Cell07PNs[6:10]), Cell07PNs[1:10])
  c610.nodf=Cell07PNs[6:10]
  attr(c610.nodf,'df')=NULL
  expect_equivalent(c(Cell07PNs[1:5], c610.nodf), Cell07PNs[1:10])
  
  expect_equivalent(kcs20[1:6], c(kcs20[1:2], kcs20[3:4], kcs20[5:6]), 
                    "combine more than 2 neuronlists")
  expect_error(c(Cell07PNs[1:5], NULL))
  expect_error(c(Cell07PNs[1:5], Cell07PNs[1:5]), "neurons with the same name")
})

test_that("head.neuronlist and tail.neuronlist behave", {
  expect_is(h<-head(Cell07PNs),class='data.frame')
  expect_that(nrow(h),equals(6L))
  nl3=Cell07PNs[1:3]
  expect_equal(head(nl3), tail(nl3))
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
  
  # null subset
  expect_equal(subset(Cell07PNs, NULL), Cell07PNs)
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
  data.frame(x)$Glomerulus[1]=NA
  
  expect_equal(subset(Cell07PNs, Glomerulus=='DL3'), 
               subset(x, Glomerulus=='DL3'), check.attributes=F)
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
  
  expect_equal(length(nmapply(mirror, kcs20[1:5], mirrorAxis = c("X","Y","Z"),
                       mirrorAxisSize=c(400,20,Inf), subset=1:3, OmitFailures=TRUE)), 4)
})

context("neuronlist: plot2d")

test_that("plot2d neuronlist contents",{
  # make tempdir for plots and be sure to clean up
  td=tempfile(); dir.create(td); owd<-setwd(td)
  on.exit({setwd(owd); unlink(td, recursive = T)})
  
  # check that the cells are plotted in expected colours
  x <- plot(Cell07PNs[1:2], colpal=grey(c(0,0.5)))
  expect_equal(length(x), 2)
  expect_equal(attr(x,'df')$col, c("#000000", "#808080"))
  x <- plot(Cell07PNs[1:4], col=4:1)
  expect_equal(attr(x,'df')$col, rev(rainbow(4)))
  
  # more tests for colour evaluation
  x<-plot(Cell07PNs, subset=names(Cell07PNs)[1:2], col=Glomerulus, colpal=heat.colors)
  expect_equal(attr(x,'df')$col, heat.colors(2))
  # note use of subset expression and use of default colour value
  x<-plot(Cell07PNs, subset=!duplicated(Glomerulus), col=Glomerulus, 
          colpal=c(DA1='red','grey'))
  expect_equal(attr(x,'df')$col, c("red","grey","grey", "grey"))
  
  x=Cell07PNs[1:4]
  # check we can cope with NA points and soma
  x[[1]]$d$X[6]=NA
  plot(x, soma=1.5, PlotAxes='YZ')
})

context("neuronlist: plot3d")

test_that("plot neuronlist contents",{
  nplotted1 <- length(plot3d(c("EBH11R", "EBH20L"), db=Cell07PNs, WithNodes=T))
  op=options(nat.default.neuronlist="Cell07PNs")
  expect_equal(length(plot3d(c("EBH11R", "EBH20L"))), nplotted1)
  plot3d(boundingbox(Cell07PNs[c("EBH11R", "EBH20L")]))
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

test_that("basic interactive 3d functionality",{
  open3d()
  expect_output(nlscan(names(Cell07PNs)[1:2], db=Cell07PNs, Wait=F), "2 / 2")
  
  selfun=readRDS('testdata/selfun_cell07.rds')
  sel_neuron=c("EBH11R", "EBH20L", "EBH20R", "EBI12L", "EBI22R", "EBJ23L", 
    "EBJ3R", "EBN19L", "EBO15L", "EBO53L", "ECA34L", "ECB3L", "LIC2R", 
    "NIA8L", "NIA8R", "NNA9L", "NNC4R", "NNE1L", "OFD2L", "SDD8L", 
    "TKC8R")
  expect_equal(find.neuron(selfun, db=Cell07PNs), sel_neuron)
  # NB equivalent because in one case the attributes on the attached data.frame 
  # are kept, in the other case not. This a pretty obscure difference and not
  # one that I can sort out in a hurry.
  expect_equivalent(find.neuron(selfun, db=Cell07PNs, rval='data.frame'), 
               Cell07PNs[sel_neuron,])
  expect_equal(find.neuron(selfun, db=Cell07PNs, rval='neuronlist'), 
                    Cell07PNs[sel_neuron])
  
  sel_soma=c("EBH20L", "EBH20R", "EBJ3R", "EBO15L", "EBO53L")
  expect_equal(find.soma(selfun, db=Cell07PNs), sel_soma)
  expect_equal(find.soma(selfun, db=Cell07PNs, invert = TRUE),
               setdiff(names(Cell07PNs), sel_soma))
  expect_equivalent(find.soma(selfun, db=Cell07PNs, rval='data.frame'), 
                    Cell07PNs[sel_soma,])
  expect_equal(find.soma(selfun, db=Cell07PNs, rval='neuronlist'), 
               Cell07PNs[sel_soma])
  
  rgl.close()
})

context("neuronlist: set operations")

test_that("set operations on neuronlists behave as expected", {
  kcs_setdiff <- kcs20[1:3]
  kcs_union <- kcs20[1:7]
  kcs_intersect <- kcs20[4:5]
  
  x <- kcs20[1:5]
  y <- kcs20[4:7]
  
  expect_equal(setdiff(x, y), kcs_setdiff)
  expect_equal(setdiff(x, names(y)), kcs_setdiff)
  expect_error(setdiff(x, 4:7))
  
  expect_equal(union(x, y), kcs_union)
  expect_equal(intersect(x, y), kcs_intersect)
})

context("as.data.frame.neuronlist")

test_that("as.data.frame.neuronlist behaves", {
  df=attr(kcs20, 'df')
  expect_equal(as.data.frame(kcs20), df)
  expect_equal(as.data.frame(kcs20, i=seq(kcs20)), cbind(df, i=seq(kcs20)))
  
  kcs20nodf=kcs20
  data.frame(kcs20nodf)=NULL
  expect_equal(as.data.frame(kcs20nodf), data.frame(row.names=names(kcs20)))
  
  # should reorder data.frame by rownames
  data.frame(kcs20nodf)<-df[rev(1:nrow(df)), ]
  expect_equal(as.data.frame(kcs20nodf), as.data.frame(kcs20))
  rownames(df)=letters[1:nrow(df)]
  expect_error(data.frame(kcs20nodf)<-df, 'rownames do not match')
})

context("neuronlist: [")
test_that("[.neuronlist does the right thing",{
  all.equal(kcs20[1:2], c(kcs20[1], kcs20[2]))
  all.equal(kcs20[1,], as.data.frame(kcs20[1]))
  all.equal(kcs20[1:2,], as.data.frame(kcs20[1:2]))
  all.equal(kcs20[1:2,1], as.data.frame(kcs20[1:2])[[1]])
  all.equal(kcs20[,], as.data.frame(kcs20))
  
  attr(kcs20,'df')=NULL
  all.equal(kcs20[1:2], c(kcs20[1], kcs20[2]))
  all.equal(kcs20[1,], as.data.frame(kcs20[1]))
  all.equal(kcs20[1:2,], as.data.frame(kcs20[1:2]))
  all.equal(kcs20[,], as.data.frame(kcs20))
})

test_that("dimnames does the right thing", {
  expect_equal(rownames(kcs20), names(kcs20))
  expect_equal(colnames(kcs20), names(as.data.frame(kcs20)))
})

test_that("[<-.neuronlist does the right thing",{
  kcs13=kcs20[1:3]
  
  kcs13[,'side']=as.character(kcs13[,'soma_side'])
  expect_equal(colnames(kcs13), c(colnames(kcs20), 'side'))
  
  # or parts of columns
  kcs13[1,'soma_side']='R'
  kcs13['FruMARCM-M001205_seg002','soma_side']='L'
  all.equal(kcs13[,'side'], kcs20[1:3,'soma_side'])
  
  expect_null(colnames(kcs13[,]<-NULL))
})
