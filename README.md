# nat: NeuroAnatomy Toolbox
[![DOI](https://img.shields.io/badge/doi-10.5281%2Fzenodo.10171-blue.svg)](http://dx.doi.org/10.5281/zenodo.10171) 
[![Release Version](https://img.shields.io/github/release/natverse/nat.svg)](https://github.com/natverse/nat/releases/latest) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat)](https://cran.r-project.org/package=nat) 
[![Build Status](https://img.shields.io/travis/natverse/nat.svg)](https://travis-ci.org/natverse/nat) 
[![Coverage Status](https://coveralls.io/repos/natverse/nat/badge.svg?branch=master)](https://coveralls.io/github/natverse/nat)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://natverse.org/nat/reference/)


An R package for the (3D) visualisation and analysis of biological image data, especially tracings of
single neurons. See [nat.examples](https://github.com/natverse/nat.examples) and [frulhns](https://github.com/jefferis/frulhns) for sample code.

**nat** is a cleaned up version of [code](https://github.com/jefferis/AnalysisSuite) that has been used in a number of papers from our group including:

[![Cell 2007 Cover](http://www.cell.com/cms/attachment/602399/4753939/cov200h.gif "Olfactory Projection Neuron Mapping")](http://dx.doi.org/10.1016/j.cell.2007.01.040)
[![CB 2010 Cover](http://www.cell.com/cms/attachment/612001/4900537/cov200h.gif "fruitless Circuit Mapping")](http://dx.doi.org/10.1016/j.cub.2010.07.045)
[![Nature 2011](http://www.nature.com/nature/journal/v478/n7368/carousel/nature10428-f4.2.jpg "Food and Pheromone Integration")](http://dx.doi.org/10.1038/nature10428)
[<img src="http://www2.mrc-lmb.cam.ac.uk/wordpress/wp-content/uploads/Switch-altered_jefferis.jpg" alt="Cell 2013 Abstract" style="height: 200px;"/>](http://dx.doi.org/10.1016/j.cell.2013.11.025)

## Quick Start

For the impatient ...

```r
# install
install.packages("nat")
# use
library(nat)

# plot some test data (?kcs20 for details)
# Drosophila Kenyon cells processed from raw data at http://flycircuit.tw
head(kcs20)
open3d()
plot3d(kcs20, col=type)
# get help
?nat
```

## Installation

A confirmed stable version of **nat** can be installed from CRAN. 

```r
install.packages("nat")
````

However, **nat** remains under quite active development, so we generally
recommend installing the latest development version directly from github using
the [devtools](https://cran.r-project.org/package=devtools) package.

```r
# install devtools if required
if (!requireNamespace("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("natverse/nat")
```

## Help
If you want some help using **nat**, then please use the following resources

* For installation issues, see the [Installation vignette](http://natverse.org/nat/articles/Installation.html)
* Start with the [overview package documentation](http://natverse.org/nat/reference/nat-package.html) (`?nat` in R)
* Thematically organised [function reference documentation](http://natverse.org/nat/reference/)
* [nat.examples](https://github.com/natverse/nat.examples) sample code
* [nat-user](https://groups.google.com/forum/#!forum/nat-user) Google group - 
  we normally respond promptly and you will also be helping future users.

## Problems
If you think that you have found a bug

* Install the development version of nat using devtools (see above)
* Check the [github issues](https://github.com/natverse/nat/issues?q=is%3Aissue) and 
  * [file a  bug report](https://github.com/natverse/nat/issues/new) if this seems to be a new problem
  * comment on an existing bug report 
* Write to the [nat-user](https://groups.google.com/forum/#!forum/nat-user) list
  for help.

Thanks for your interest in *nat*!
