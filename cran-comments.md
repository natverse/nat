## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* winbuilder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There were 2 NOTEs on winbuilder:

http://win-builder.r-project.org/8B95Jeab8U8P/

* Possibly mis-spelled words in DESCRIPTION:
  (false positive)

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped


## Downstream dependencies
I have also run R CMD check on downstream dependencies of nat. All packages 
passed.
