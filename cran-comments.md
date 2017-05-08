# nat 1.8.9
* minor updates and bug fixes
## Test environments
* local OS X install, R 3.3.3
* ubuntu 14.04 (on travis-ci), R 3.4.0
* winbuilder (devel)

## R CMD check results
Running R CMD check --as-cran gave

no ERRORs, WARNINGs or NOTEs:

Winbuilder also gave one note:

https://win-builder.r-project.org/3Ws3hYsNBos9/00check.log

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gregory Jefferis <jefferis@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  AmiraMesh (13:25)
  LineSet (15:5)
  NRRD (13:8)
  NeuroAnatomy (3:8, 11:14)
  SWC (14:64)
  SkeletonGraph (15:17)
  hxsurf (13:73)
  nat (11:36)
  skeletonisation (18:63)

  
but I believe that these are all false positives and there are no additions to 
this list since the last CRAN release.

## Tests
Note that some tests have been marked as donttest to reduce the standard test
time including on CRAN. I have succesfully run all tests using 

  R CMD CHECK --run-donttest --as-cran
