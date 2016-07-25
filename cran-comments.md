# nat 1.8.4
nat 1.8.4 fixes a breakage due to changes in behaviour for testthat >0.12 as 
well as providing some new functionality.

## Test environments
* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.2.5
* winbuilder (devel and release)

http://win-builder.r-project.org/UwuUTD6g7r0B/

## R CMD check results
Running R CMD check --as-cran gave

no ERRORs or WARNINGs and 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Greg Jefferis <jefferis@gmail.com>’

Found the following (possibly) invalid URLs:
  URL: http://dx.doi.org/10.1038/nature10428 (moved to /nature/journal/v478/n7368/full/nature10428.html)
    From: README.md
    Status: 401
    Message: Unauthorized

I have left this dx.doi.org URL in place since it will always redirect to the 
current location of the article on the Nature website (which is not guaranteed).
The Unauthorized message comes up because the article is paywalled (I do not see
it at work).

Winbuilder also came up with one note:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Greg Jefferis <jefferis@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  AmiraMesh (11:25)
  LineSet (13:5)
  NRRD (11:8)
  NeuroAnatomy (3:8, 9:14)
  SWC (12:64)
  SkeletonGraph (13:17)
  hxsurf (11:73)
  nat (9:36)
  skeletonisation (16:63)
  
but I believe that these are all false positives.

## Tests
Note that some tests have been marked as donttest to reduce the standard test
time including on CRAN. I have succesfully run all tests using 

  R CMD CHECK --run-donttest --as-cran
