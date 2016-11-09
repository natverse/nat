# nat 1.8.7
nat 1.8.7 fixes a breakage due to changes in behaviour of rep.int in svn 
changeset c71611 that was resulting in build errors of the current CRAN release
(v1.8.4) under r-devel. There are numerous additional bug fixes and enhancements
since v1.8.4, including two vignettes.

## Test environments
* local OS X install, R 3.3.2-patched
* ubuntu 12.04 (on travis-ci), R 3.3.1
* winbuilder (devel)

## R CMD check results
Running R CMD check --as-cran gave

no ERRORs, WARNINGs or NOTEs:

Winbuilder also gave one note:

https://win-builder.r-project.org/yji0dXWrOkVq

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Greg Jefferis <jefferis@gmail.com>'

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
