context("neuronlist")

test_that("head.neuronlist behaves", {
  expect_is(h<-head(Cell07PNs),class='data.frame')
  expect_that(nrow(h),equals(6L))
})

test_that("with.neuronlist / droplevels behave", {
  expect_that(with(Cell07PNs,length(unique(Glomerulus))),equals(4L))
  expect_that(nlevels(droplevels(Cell07PNs)$Glomerulus),equals(4L))
})
