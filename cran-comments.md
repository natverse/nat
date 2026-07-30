# nat 1.8.26

This submission fixes the NOTE reported on the CRAN check page, replacing calls
to structure() that used the deprecated special names .Names, .Dim and .Dimnames
with names, dim and dimnames respectively.

With many thanks,

Greg Jefferis.

## Test environments

* local OS X install, R 4.5.1.
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

https://win-builder.r-project.org/
