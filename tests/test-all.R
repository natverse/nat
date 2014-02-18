library(testthat)
library(nat)

if(Sys.getenv('NOT_CRAN') == "true") {
  # Run all tests
  test_package("nat")
} else {
  # We're on CRAN, so don't run anything involving remote files
  regex <- "^(?:(?!nocran).)+$"
  attr(regex, "perl") <- TRUE
  test_package("nat", filter=regex)
}
