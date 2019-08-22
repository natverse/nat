# nat 1.8.13

This is a maintenance release to fix two key issues. I hope to make
a full-featured release that will also address a NOTE due to a change
in rgl::as.mesh3d on my return from vacation in early September
(discussed with Kurt Hornik).

* fix for a digest mismatch in a test causing a check error on CRAN
* fix for very slow connection handling in neuronlistfh (change in behaviour 
  since R 3.6.0)

## Test environments
* local OS X install, R 3.6.1
* ubuntu 12.04 (on travis-ci), R 3.6.1
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://win-builder.r-project.org/2z5JKK1sfPfL/00check.log

Please note that previous versions of R reported possible spelling errors in 
the DESCRIPTION file, but these were all false positives.

## Tests
Note that some tests have been marked as donttest to reduce the standard test
time including on CRAN. I have succesfully run all tests using 

  R CMD CHECK --run-donttest --as-cran
