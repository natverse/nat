## Test environments
* local OS X install, R 3.2.0
* ubuntu 12.04 (on travis-ci), R 3.2.0
* winbuilder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs on any platform.

There was 1 NOTE on winbuilder release:

http://win-builder.r-project.org/BODOoUYV1PhZ/00check.log

Possibly mis-spelled words in DESCRIPTION:

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
