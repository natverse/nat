# nat 1.8.22

This submission is to recover from the archival of nat due to its strong
dependency nat.utils being archived. A few changes were required to handle 
changes to R CMD check. The only residual NOTE on win builder relates to this 
archival and some false positive "Possibly misspelled words in DESCRIPTION".

With many thanks,

Greg Jefferis.

## Test environments

* local OS X install, R 4.2.2.
* win-builder (r-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

https://win-builder.r-project.org/0F3TYoe3QYE7/00check.log

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  AmiraMesh (15:25)
  LineSet (17:5)
  NRRD (15:8)
  NeuroAnatomy (3:8, 13:14)
  SWC (16:64)
  SkeletonGraph (17:17)
  hxsurf (15:73)
  nat (13:36)
  skeletonisation (20:63)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-06-05 as requires archived package
    'nat.utils'.
