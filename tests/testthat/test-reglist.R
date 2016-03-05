context("reglist")
test_that('We can simplify reglists',{
  reg="testdata/cmtk/FCWB_mirror_level-01.list"
  rl=reglist(t(rgl::rotationMatrix(0)),
          t(rgl::translationMatrix(1,2,3)),
          reg)
  expect_is(rl, 'reglist')
  expect_equal(length(rl), 3L)
  expect_equal(simplify_reglist(rl, as.cmtk = FALSE), rl)
  expect_is(srl<-simplify_reglist(rl, as.cmtk = T), 'cmtkreg')
  expect_equal(length(srl), 3L)
  expect_equal(srl[3], rl[[3]])
  # the default should convert to cmtk in this case
  expect_is(simplify_reglist(rl), "cmtkreg")
  
  # this can't be simplified
  rl[[4]]=function(x, ...) x
  expect_error(simplify_reglist(rl, as.cmtk = TRUE), "cannot convert.*CMTK")
  
  # check that we can invert affine registrations
  # affine followed by inverse should give identity
  rl2=reglist(c(rl[2],rl[2]), swap=c(F,T))
  expect_equal(simplify_reglist(rl2), reglist(rgl::identityMatrix()))
})

test_that("we can combine reglists",{
  I=diag(4)
  S=I
  diag(S)=c(1, 2, 3, 1)
  rl=reglist(S, I)
  expect_equal(c(reglist(S), reglist(I)), rl)
  p='path/to/my/reg.list'
  rl2=c(rl, p)
  rl3=
  expect_equal(c(rl, p), reglist(S, I, p))
  expect_equal(c(reglist(p), rl), reglist(p, S, I))
})
