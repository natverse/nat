# nat 1.8.21

This is a maintenance release to fix two issues introduced by changes to 
dependencies.

1. deprecation of some rgl.* functions by rgl v1.0.1
2. changes to handling of attributes in a forthcoming update of igraph (v1.3.6)

In addition the NEWS file is tweaked to fix a NOTE identified by the cran pretest
check. Strangely this error only appeared on that linux platform

https://win-builder.r-project.org/incoming_pretest/nat_1.8.20_20230128_210727/Debian/00check.log

## Test environments

* local OS X install, R 4.2.2.
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://win-builder.r-project.org/wZ0S41m2kNkU/00check.log

