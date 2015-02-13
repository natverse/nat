context("potential synapses")

test_that("can call potential synapses on some neurons",{
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2)
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[1]], s=2)
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, method='approx')
})

test_that("can calculat potential synapses with bounding box",{
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, 
                     bounds=c(250, 320, 60, 130, 90, 160))
  potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, 
                     bounds=c(250, 320, 60, 130, 90, 160), method='approx')
  
  expect_equal(potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, 
                                  bounds=rep(0, 6)),
               0)
  expect_equal(potential_synapses(Cell07PNs[[1]], Cell07PNs[[2]], s=2, 
                                  bounds=rep(0, 6), method='approx'), 
               0)
})

test_that("can call potential synapses on neuron list",{
  expect_is(m1<-potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2),
            'matrix')
  
  expect_equal(names(Cell07PNs)[1:3],colnames(m1))
  expect_equal(names(Cell07PNs)[4:6],rownames(m1))
  
  expect_equal(potential_synapses(Cell07PNs[[1]], Cell07PNs[[4]], s=2),
               m1[1,1])
  
  potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2, method='approx')
})
