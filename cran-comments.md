# nat 1.8.18

This is a maintenance release to fix an issue with building vignettes on 
platforms without pandoc. Note that this follows only a couple of days after nat
1.8.17 which fixed an issue with rgl's recent dependence on the webshot2 package
but failed to  handle fully situations when pandoc is unavailable (for which my
apologies).

## Test environments

* local OS X install, R 4.1.1
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

https://win-builder.r-project.org/1O9m1mL5531s/

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
