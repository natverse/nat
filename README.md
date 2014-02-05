# nat: NeuroAnatomy Toolbox
An R package for the analysis of biological image data, especially tracings of
single neurons.

## Installation
As of v1.0 there is a released version on CRAN.

```r
install.packages('nat')
```

It you wish to run the package tests, it is necessary to install with 
`dependencies=TRUE`.

### Released versions
Interim **source code** packages for released versions are available from our 
lab repository:

```r
# when binary packages are preferred (e.g. mac/windows)
install.packages('nat',repos=c('http://jefferislab.org/R',getOption("repos")),
                 type='both')
# when source packages are the default (e.g. linux)
install.packages('nat',repos=c('http://jefferislab.org/R',getOption("repos")))
```

Note the specification of both the jefferislab.org repository and the default 
CRAN repository in order to ensure that package dependencies are installed from 
CRAN and the main package is installed from our repository. Note also that it is
necessary to specify `type='both` on platforms where binary packages are the
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
Windows, Linux and Mac at http://www.nitrc.org/projects/cmtk/. Source code is 
available from the same site or an unofficial mirror repository at 
https://github.com/jefferis/cmtk. We have extensive experience of using CMTK
under linux (where we compile from source) and mac (where we compile or use the
MacOSX-10.6-x86_64.dmg binary installers).
