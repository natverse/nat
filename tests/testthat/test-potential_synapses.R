context("potential synapses")

test_that("can call potential synapses on some neurons",{
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2)
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, method='approx')
})

test_that("can call potential synapses on neuron list",{
  expect_is(m1<-potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2), 'matrix')
  potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2, method='approx')
})
