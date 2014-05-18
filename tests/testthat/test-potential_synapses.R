test_that("can call potential synapses on some neurons",{
  PotentialSynapses.neuron(Cell07PNs[[1]], Cell07PNs[[2]])
  PotentialSynapses.neuron(Cell07PNs[[1]], Cell07PNs[[2]], method='approx')
})