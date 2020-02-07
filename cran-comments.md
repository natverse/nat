# nat 1.8.14

This is a maintenance release to fix a failing test on CRAN when the suggested
Rvcg package is unavailable (thanks to BDR).


## Test environments
* local OS X install, R 3.6.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.2
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://travis-ci.org/natverse/nat/builds/647101233

Please note that previous versions of R reported possible spelling errors in 
the DESCRIPTION file, but these were all false positives.

## Tests
Note that some tests have been marked as donttest to reduce the standard test
time including on CRAN. I have succesfully run all tests using 

  R CMD CHECK --run-donttest --as-cran
