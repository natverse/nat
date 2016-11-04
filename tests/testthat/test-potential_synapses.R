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
  baseline = matrix(
    c(228L, 268L, 140L, 95L, 285L, 323L, 184L, 256L, 144L),
    ncol = 3,
    dimnames = list(c("EBI12L", "EBI22R", "EBJ23L"),
                    c("EBH11R", "EBH20L", "EBH20R"))
  )
  
  expect_equal(m1<-potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2),
               baseline)
  
  expect_equal(names(Cell07PNs)[1:3],colnames(m1))
  expect_equal(names(Cell07PNs)[4:6],rownames(m1))
  
  expect_equal(potential_synapses(Cell07PNs[[1]], Cell07PNs[[4]], s=2),
               m1[1,1])
  
  potential_synapses(Cell07PNs[1:3], Cell07PNs[4:6], s=2, method='approx')
})
