#!/usr/bin/Rscript --no-restore --slave
options(repos="https://cran.rstudio.com/")
options(pager='cat')
message("interactive is ", interactive())
library(pkgdown)
args=commandArgs(trailingOnly = TRUE)
if(!length(args)) args="."
for(d in args) {
  build_site(d)
}
