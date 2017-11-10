# nat 1.8.11

* minor updates and fixes
* includes fix for upcoming testthat v2 at request of Hadley Wickham

## Test environments
* local OS X install, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://win-builder.r-project.org/2x7Txk57YxtD/00check.log

Please note that previous versions of R reported possible spelling errors in 
the DESCRIPTION file, but these were all false positives.

## Tests
Note that some tests have been marked as donttest to reduce the standard test
time including on CRAN. I have succesfully run all tests using 

  R CMD CHECK --run-donttest --as-cran
