# nat 1.8.16

This is a maintenance release to fix an issue noted by BRD in several pasckages (including BIGL RRphylo Rpolyhedra bcROCsurface drugCombo gMOIP hyperoverlap matlib mgcViz nat) that resulted from changes in 
behaviour of the rgl package. This now defaults to using the off-CRAN webshot2 package to generate snapshots/interactive 3D scenes. The changes now suggest webshot2 and list a drat repository by Duncan Murdoch to provide it.

## Test environments

* local OS X install, R 4.1.1
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

https://win-builder.r-project.org/PW5v13v9Nu13/00check.log

### NOTEs

Besides the standard:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gregory Jefferis <jefferis@gmail.com>'

the only other note refers to the drat repository discussed above:

Suggests or Enhances not in mainstream repositories:
  webshot2
Availability using Additional_repositories specification:
  webshot2   yes   https://dmurdoch.github.io/drat
  
this appears to be acceptable, since the [rgl package](https://cran.r-project.org/package=rgl) and multiple reverse dependencies on CRAN (e.g. https://cran.r-project.org/package=mgcViz) have successfully used this approach.
