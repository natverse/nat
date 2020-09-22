# nat 1.8.15

This is a maintenance release to fix a failing test on R-devel (R 4.1). This
updates the expectation to account for new behaviour of the c() generic
function. See https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17900

## Test environments
* local OS X install, R 4.0.2 and 4.1.0
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://win-builder.r-project.org/5a09qIbsh1J5/00check.log
