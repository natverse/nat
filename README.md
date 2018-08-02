# nat: NeuroAnatomy Toolbox
[![DOI](https://img.shields.io/badge/doi-10.5281%2Fzenodo.10171-blue.svg)](http://dx.doi.org/10.5281/zenodo.10171) 
[![Release Version](https://img.shields.io/github/release/jefferis/nat.svg)](https://github.com/jefferis/nat/releases/latest) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat)](https://cran.r-project.org/package=nat) 
[![Build Status](https://img.shields.io/travis/jefferis/nat.svg)](https://travis-ci.org/jefferis/nat) 
[![Coverage Status](https://coveralls.io/repos/jefferis/nat/badge.svg?branch=master)](https://coveralls.io/r/jefferis/nat?branch=master)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferis.github.io/nat/reference/)


An R package for the (3D) visualisation and analysis of biological image data, especially tracings of
single neurons. **nat** is the core package of a wider suite of neuroanatomy 
tools introduced at http://jefferislab.github.io. **nat** (and its [ancestors](https://github.com/jefferis/AnalysisSuite))
have been used in a number of papers from our group including:

[![Cell 2007 Cover](http://www.cell.com/cms/attachment/602399/4753939/cov200h.gif "Olfactory Projection Neuron Mapping")](http://dx.doi.org/10.1016/j.cell.2007.01.040)
[![CB 2010 Cover](http://www.cell.com/cms/attachment/612001/4900537/cov200h.gif "fruitless Circuit Mapping")](http://dx.doi.org/10.1016/j.cub.2010.07.045)
[![Nature 2011](https://media.nature.com/full/nature-assets/nature/journal/v478/n7368/carousel/nature10428-f4.2.jpg "Food and Pheromone Integration")](http://dx.doi.org/10.1038/nature10428)
[<img src="http://www2.mrc-lmb.cam.ac.uk/wordpress/wp-content/uploads/Switch-altered_jefferis.jpg" alt="Cell 2013 Abstract" style="height: 200px;"/>](http://dx.doi.org/10.1016/j.cell.2013.11.025)
[![Neuron 2016](http://www2.mrc-lmb.cam.ac.uk/wordpress/wp-content/uploads/P1Clustering_200_sq_crop-215x215.jpg "NBLAST")](http://dx.doi.org/10.1016/j.neuron.2016.06.012)


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

However, **nat** remains under quite active development, so if you will be 
using **nat** extensively, we generally
recommend installing the latest development version directly from github using
the [devtools](https://cran.r-project.org/package=devtools) package.

```r
# install devtools if required
if (!requireNamespace("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("jefferis/nat")

```
## Learn

To get an idea of what **nat** can do:

* Skim through the Articles listed at http://jefferis.github.io/nat/
* Take a look at the [R Markdown](http://rmarkdown.rstudio.com/) reports used 
  to generate the [figures for our NBLAST paper](http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/paper/).

When you're ready to learn more:

* Read the [overview package documentation](http://jefferis.github.io/nat/reference/nat-package.html)
  (`?nat` in R)
* Read the [Introduction to neurons](http://jefferis.github.io/nat/articles/neurons-intro.html) article
* Check out the thematically organised [function reference documentation](http://jefferis.github.io/nat/reference/). 
  * Most help pages include examples.
* Try out sample code:
  - [nat.examples](https://github.com/jefferis/nat.examples) has detailed 
    examples for data sets from a range of model organisms and techniques
  - [frulhns](https://github.com/jefferis/frulhns) analysis of sexually dimorphic circuits
  - [NBLAST figures](https://github.com/jefferislab/NBLAST_figures/)

## Help
If you want some help using **nat**:

* For installation issues, see the [Installation vignette](http://jefferis.github.io/nat/articles/Installation.html)
* Contact [nat-user](https://groups.google.com/forum/#!forum/nat-user) Google group - 
  we normally respond promptly and you will also be helping future users.

If you think that you have found a bug:

* Install the development version of nat using devtools (see above) and see if 
  that helps.
* Check the [github issues](https://github.com/jefferis/nat/issues?q=is%3Aissue) and 
  - [file a  bug report](https://github.com/jefferis/nat/issues/new) if this seems to be a new problem
  - comment on an existing bug report 

Thanks for your interest in **nat**!
