library(testthat)
library(nat)
library(httr)

# Is internet accessible?
internet.ok=isTRUE(try(url_ok('http://flybrain.mrc-lmb.cam.ac.uk/')))

if(Sys.getenv('NOT_CRAN') == "true" && internet.ok) {
  # note that we want to run all tests requiring internet access
  Sys.setenv(NAT_INTERNET_TESTS="TRUE")
  # Run all test files
  test_check("nat")
} else {
  # We're on CRAN or flybrain is inacessible, so don't run anything involving 
  # remote files
  Sys.setenv(NAT_INTERNET_TESTS="")
  test_check("nat", filter="^[^.]+")
}
