context("summary")

test_that("summary.neuronlist behaves", {
  expect_is(s <- summary(Cell07PNs), 'data.frame')
  expect_known_value(s, file = 'testdata/summary_cell07pns.rda')
  expect_is(summary(kcs20, veclength=1.2), 'data.frame')
})
