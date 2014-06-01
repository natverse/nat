library(testthat)
library(nat)
library(httr)

if(Sys.getenv('NOT_CRAN') == "true" && 
     isTRUE(try(url_ok('http://flybrain.mrc-lmb.cam.ac.uk/')))) {
  # Run all tests
  test_check("nat")
} else {
  # We're on CRAN or flybrain is inacessible, so don't run anything involving 
  # remote files
  regex <- "^[^.]+"
  test_check("nat", filter=regex)
}
