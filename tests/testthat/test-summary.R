context("summary")

test_that("summary.neuronlist behaves", {
  expect_is(s <- summary(Cell07PNs), 'data.frame')
  expect_equal_to_reference(s, file = 'testdata/summary_cell07pns.rda', tolerance=1e-3)
  expect_is(summary(kcs20, veclength=1.2), 'data.frame')
})
