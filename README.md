# nat: NeuroAnatomy Toolbox
[![Build Status](https://travis-ci.org/jefferis/nat.svg)](https://travis-ci.org/jefferis/nat)

An R package for the (3D) visualisation and analysis of biological image data, especially tracings of
single neurons. See [nat.examples](https://github.com/jefferis/nat.examples) and [frulhns](https://github.com/jefferis/frulhns) for sample code.

**nat** is a cleaned up version of [code](https://github.com/AnalysisSuite) that has been used in a number of papers from our group including:

[![Cell 2007 Cover](http://www.cell.com/cms/attachment/602399/4753939/cov200h.gif "Olfactory Projection Neuron Mapping")](http://dx.doi.org/10.1016/j.cell.2007.01.040)
[![CB 2010 Cover](http://www.cell.com/cms/attachment/612001/4900537/cov200h.gif "fruitless Circuit Mapping")](http://dx.doi.org/10.1016/j.cub.2010.07.045)
[![Nature 2011](http://www.nature.com/nature/journal/v478/n7368/carousel/nature10428-f4.2.jpg "Food and Pheromone Integration")](http://dx.doi.org/10.1038/nature10428)
[<img src="http://www2.mrc-lmb.cam.ac.uk/wordpress/wp-content/uploads/Switch-altered_jefferis.jpg" alt="Cell 2013 Abstract" style="height: 200px;"/>](http://dx.doi.org/10.1016/j.cell.2013.11.025)

Quick Start
===========

For the impatient ...

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

A larger data set, 300 olfactory projection neurons from [Grosjean et al 2011](http://flybrain.mrc-lmb.cam.ac.uk/dokuwiki/doku.php?id=si:grosjean_and_silbering_2011)

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

# Details
## Installation
As of v1.0 there is a released version on CRAN.

```r
install.packages("nat")
```

It you wish to run the package tests, it is necessary to install with 
`dependencies=TRUE`.

### Released versions
Interim **source code** packages for released versions are available from our 
lab repository:

```r
# when binary packages are preferred (e.g. mac/windows)
install.packages("nat",repos=c("http://jefferislab.org/R",getOption("repos")),
                 type="both")
# when source packages are the default (e.g. linux)
install.packages("nat",repos=c("http://jefferislab.org/R",getOption("repos")))
```

Note the specification of both the jefferislab.org repository and the default 
CRAN repository in order to ensure that package dependencies are installed from 
CRAN and the main package is installed from our repository. Note also that it is
necessary to specify `type="both"` on platforms where binary packages are the
norm (Windows/MacOS X) since **nat** is only provided as a source package on our
repository.

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferis/nat/tarball/master),
and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")
devtools::install_github("nat", "jefferis")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## External Dependencies
**nat** is self sufficient for core functionality, but the transformation of 3d
data using Computational Morphometry Toolkit (CMTK) registrations depends on an
external installation of that toolkit. CMTK binaries can be downloaded for
Windows, Linux and Mac at <http://www.nitrc.org/projects/cmtk/>. Source code is 
available from the same site or an unofficial mirror repository at 
<https://github.com/jefferis/cmtk>. We have extensive experience of using CMTK
under linux (where we compile from source) and mac (where we compile or use the
MacOSX-10.6-x86_64.dmg binary installers). We have also used 
[neurodebian](http://neuro.debian.net/pkgs/cmtk.html) to install as part of the
travis continuous integration setup (see the project's [.travis.yml](https://github.com/jefferis/nat/blob/master/.travis.yml) file).
