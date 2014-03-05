library(testthat)
library(nat)

if(Sys.getenv('NOT_CRAN') == "true") {
  # Run all tests
  test_check("nat")
} else {
  # We're on CRAN, so don't run anything involving remote files
  regex <- "^[^.]+"
  test_check("nat", filter=regex)
}
