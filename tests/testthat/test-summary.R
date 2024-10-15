context("summary")

test_that("summary.neuronlist behaves", {
  expect_is(s <- summary(Cell07PNs), 'data.frame')
  expect_known_value(s, file = test_path('testdata/summary_cell07pns.rds'), tolerance=1e-3)
  expect_is(summary(kcs20, veclength=1.2), 'data.frame')
  
  skip_if_not(use_natcpp())
  op <- options('nat.use_natcpp'=FALSE)
  on.exit(options(op))
  # The natcpp method matches the old one
  # a discrepancy masked by tolerance = 1e-3
  # this is down to some neurons having cycles
  expect_equal(summary(Cell07PNs), s, tolerance = 1e-3)
  expect_known_value(summary(Cell07PNs), 
                     file = test_path('testdata/summary_cell07pns.rds'),
                     tolerance=1e-3)
})

context('print')

test_that("print.neuronlist behaves", {
  expect_output(print(Cell07PNs),
    regexp = "'neuronlist' containing 40 'neuron'.*27 vars")
  expect_output(print(Cell07PNs[1]),
                regexp = "'neuronlist' containing 1 'neuron' object ")
})

test_that("print.neuron behaves", {
  expect_output(print(Cell07PNs[[1]]),
                regexp = "'neuron' with 180 vertices")
})

test_that("print.dotprops behaves", {
  expect_output(print(kcs20[[1]]),
                regexp = "'dotprops' object with 284 vertices")
  x=kcs20[[1]]
  x$points=x$points[1,,drop=FALSE]
  expect_output(print(x), regexp = " 1 vertex")
})
