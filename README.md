# nat: NeuroAnatomy Toolbox
[![DOI](https://img.shields.io/badge/doi-10.5281%2Fzenodo.10171-blue.svg)](http://dx.doi.org/10.5281/zenodo.10171) 
[![Release Version](https://img.shields.io/github/release/jefferis/nat.svg)](https://github.com/jefferis/nat/releases/latest) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/nat)](http://cran.r-project.org/package=nat) 
[![Build Status](https://img.shields.io/travis/jefferis/nat.svg)](https://travis-ci.org/jefferis/nat) 
[![Coverage Status](https://coveralls.io/repos/jefferis/nat/badge.svg?branch=master)](https://coveralls.io/r/jefferis/nat?branch=master)

An R package for the (3D) visualisation and analysis of biological image data, especially tracings of
single neurons. See [nat.examples](https://github.com/jefferis/nat.examples) and [frulhns](https://github.com/jefferis/frulhns) for sample code.

**nat** is a cleaned up version of [code](https://github.com/jefferis/AnalysisSuite) that has been used in a number of papers from our group including:

[![Cell 2007 Cover](http://www.cell.com/cms/attachment/602399/4753939/cov200h.gif "Olfactory Projection Neuron Mapping")](http://dx.doi.org/10.1016/j.cell.2007.01.040)
[![CB 2010 Cover](http://www.cell.com/cms/attachment/612001/4900537/cov200h.gif "fruitless Circuit Mapping")](http://dx.doi.org/10.1016/j.cub.2010.07.045)
[![Nature 2011](http://www.nature.com/nature/journal/v478/n7368/carousel/nature10428-f4.2.jpg "Food and Pheromone Integration")](http://dx.doi.org/10.1038/nature10428)
[<img src="http://www2.mrc-lmb.cam.ac.uk/wordpress/wp-content/uploads/Switch-altered_jefferis.jpg" alt="Cell 2013 Abstract" style="height: 200px;"/>](http://dx.doi.org/10.1016/j.cell.2013.11.025)

Quick Start
===========

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

A larger data set, 300 olfactory projection neurons from [Grosjean et al 2011](http://flybrain.mrc-lmb.cam.ac.uk/dokuwiki/doku.php?id=si:grosjean_and_silbering_2011):

```r
load(url("http://flybrain.mrc-lmb.cam.ac.uk/si/grosjean11/MyNeuronsFCIR.rda"))
plot3d(MyNeurons[[1]])
clear3d()
head(MyNeurons)

# 3d plot of neurons from olfactory glomeruli beginning DM
# coloured by glomerulus
rval=plot3d(MyNeurons, subset=grepl("^DM",Glomerulus), col=factor(Glomerulus),
  lwd=2, WithNodes=FALSE)
# make a legend so that you know which colours match which glomerulus
with(attr(rval,'df'), legend('center', legend = unique(Glomerulus), fill=unique(col)))

# more help
?plot3d.neuronlist
?subset.neuronlist
```

# Details
## Prerequisites
**nat** is an R package and therefore runs on Mac/Linux/Windows. The only pre-requisite for most functionality is a recent version of R (>=3.1.0 recommended).

* http://www.r-project.org

3D visualisation is provided by the rgl package based on OpenGL. On Mac OS X if 
you use RStudio or R from the terminal, you must have a copy of XQuartz, the X11
window manager, installed. This is no longer a default install since Mac OS X 
10.8, but the OS should offer to it install it for you if something tries to use
it. Alternatively you can get it directly from 
https://xquartz.macosforge.org/landing/. This page is also linked from the 
[Download R for (Mac) OS X](http://cran.r-project.org/bin/macosx/) page.

If you want to apply non-rigid registrations calculated by the Computational Morphometry Toolkit (CMTK) you will need to install that separately – see section *External Dependencies* below. 

## Installation
As of v1.0 there is a released version on CRAN. This is normally updated only
every few months.

```r
install.packages("nat")
```

If you wish to run the package tests, it is necessary to install with all dependencies:

```r
install.packages("nat", dependencies=TRUE)
```

### Development version
**nat** remains under quite active development, so you may wish to install the
development version directly from github. The recommended way to do this is to
install Hadley Wickham's invaluable [devtools](http://CRAN.R-project.org/package=devtools)
package (if you have not already done so) and then use that to install nat.

```r
# install devtools if required
if (!require("devtools")) install.packages("devtools")
# then install nat
devtools::install_github("jefferis/nat")
```

The **nat** package includes extensive unit tests which are run along with R's
(extremely fastidious) package check routines by the [Travis](http://travis-ci.org/jefferis/nat)
continuous integration server. The master branch is therefore considered very stable
and may well contain fixes or enhancements over released versions. 
However, you can install the latest point release version as follows:

```r
devtools::install_github("jefferis/nat",ref="release")
```
The same syntax can be used to install any arbitrary version that you want 
from github. See `?install_github` for details.

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) to
install in this way, but devtools should offer to install this for you if you
do not already have it.

## External Dependencies
**nat** is self sufficient for core functionality, but the transformation of 3D
data using Computational Morphometry Toolkit (CMTK) registrations depends on an
external installation of that toolkit. CMTK binaries can be downloaded for
Windows, Linux and Mac at <http://www.nitrc.org/projects/cmtk/>. Source code is 
available from the same site or an unofficial mirror repository at 
<https://github.com/jefferis/cmtk>. We have extensive experience of using CMTK
under Linux (where we compile from source) and Mac (where we compile or use the
MacOSX-10.6-x86_64.dmg binary installers). We have also used 
[neurodebian](http://neuro.debian.net/pkgs/cmtk.html) to install as part of the
Travis continuous integration setup (see the project's [.travis.yml](https://github.com/jefferis/nat/blob/master/.travis.yml) file).
