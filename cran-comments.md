# nat 1.8.1
nat 1.8.1 fixes some non-canonical URLs to CRAN packages spotted by Uwe Ligges 
in 1.8.0

## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* winbuilder (devel and release)

http://win-builder.r-project.org/Gm9K0rOu3yGr/

## R CMD check results
Running R CMD check --as-cran gave

no ERRORs or WARNINGs and 1 NOTE:

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped

## Downstream dependencies
I have also run R CMD check on downstream dependencies of nat. All packages 
passed.
