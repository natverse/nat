context("summary")

test_that("summary.neuronlist behaves", {
  expect_is(s <- summary(Cell07PNs), 'data.frame')
  expect_equal_to_reference(s, file = 'testdata/summary_cell07pns.rda', tolerance=1e-3)
  expect_is(summary(kcs20, veclength=1.2), 'data.frame')
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
