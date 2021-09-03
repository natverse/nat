# nat 1.8.16

This is a maintenance release to fix an issue noted by BDR in several packages (including BIGL RRphylo Rpolyhedra bcROCsurface drugCombo gMOIP hyperoverlap matlib mgcViz nat) that resulted from changes in behaviour of the rgl package. This now defaults to using the off-CRAN webshot2 package to generate snapshots/interactive 3D scenes. The changes to nat now suggest webshot2 and list an external drat repository by Duncan Murdoch to provide it. This strategy was recommended by Duncan Murdoch and is currently used for the rgl package itself.

## Test environments

* local OS X install, R 4.1.1
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

https://win-builder.r-project.org/j3MBn25qp2tH/00check.log

### NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gregory Jefferis <jefferis@gmail.com>'

Suggests or Enhances not in mainstream repositories:
  webshot2
Availability using Additional_repositories specification:
  webshot2   yes   https://dmurdoch.github.io/drat

* checking package dependencies ... NOTE
Package suggested but not available for checking: 'webshot2'
  
this use of a drat repository appears to be acceptable, since the [rgl package](https://cran.r-project.org/package=rgl) and multiple reverse dependencies on CRAN (e.g. https://cran.r-project.org/package=mgcViz) have successfully used this approach.
