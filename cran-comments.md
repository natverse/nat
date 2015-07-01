# nat 1.7.0
nat 1.7.0 fixes an incompatibility with the recently updated igraph 1.0
that is resulting in build errors for nat 1.6.6 
(see http://cran.r-project.org/web/checks/check_results_nat.html) 
as well as adding significant new functionality.

## Test environments
* local OS X install, R 3.2.1
* ubuntu 12.04 (on travis-ci), R 3.2.1
* winbuilder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There were 2 NOTEs on winbuilder release:

http://win-builder.r-project.org/S5289h1Fn4JY/00check.log

NOTE 1: No repository set, so cyclic dependency check skipped

NOTE 2: Possibly mis-spelled words in DESCRIPTION:

These are either:

1. Proper names describing data formats:
  AmiraMesh (11:25)
  LineSet (13:5)
  NRRD (11:8)
  SWC (12:64)
  SkeletonGraph (13:17)
  hxsurf (11:73)

2. Part of the explanation of the package name:
  NeuroAnatomy (3:8, 9:14)
  nat (9:36)

3. words not in the dictionary
  skeletonisation (16:63)

Note that all other package names/external software have been capitalised in
response to Uwe Ligges' evaluation of my submission of version 1.6.5. Note that
there is one unquoted occurrence of 'nat' (this package's name) at the start of
the description as part of a phrase clarifying the origin of the package name.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of nat. All packages 
passed.
