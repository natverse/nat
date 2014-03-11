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
