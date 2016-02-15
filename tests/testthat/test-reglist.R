context("reglist")
test_that('We can simplify reglists',{
  reg="testdata/cmtk/FCWB_mirror_level-01.list"
  rl=reglist(t(rgl::rotationMatrix(0)),
          t(rgl::translationMatrix(1,2,3)),
          reg)
  expect_is(rl, 'reglist')
  expect_equal(length(rl), 3L)
  expect_equal(simplify_reglist(rl), rl)
  expect_is(srl<-simplify_reglist(rl, as.cmtk = T), 'cmtkreg')
  expect_equal(length(srl), 3L)
  expect_equal(srl[3], rl[[3]])
  
  # this can't be simplified
  rl[[4]]=function(x, ...) x
  expect_error(simplify_reglist(rl, as.cmtk = TRUE), "cannot convert.*CMTK")
})
