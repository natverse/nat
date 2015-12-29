context("summary")

test_that("summary.neuronlist behaves", {
  expect_is(summary(Cell07PNs), 'data.frame')
  expect_is(summary(kcs20, veclength=1.2), 'data.frame')
})
