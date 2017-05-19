#!/usr/local/bin/Rscript --no-restore --slave
options(repos="https://cran.rstudio.com/")
options(pager='cat')
# we don't want spurious progress bars everywhere
options(nat.progress="none")
message("interactive is ", interactive())
library(pkgdown)
args=commandArgs(trailingOnly = TRUE)
if(!length(args)) args="."
for(d in args) {
  build_site(d)
}
