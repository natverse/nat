# nat: NeuroAnatomy Toolbox
An R package for the analysis of biological image data, especially tracings of
single neurons.

## Installation
As of v1.0 there is a released version on CRAN.

```r
install.packages('nat',dependencies=TRUE)
```

### Released versions
Interim released versions are available from our lab repository:

```r
install.packages('nat',repos=c(getOption("repos"),'http://jefferislab.org/R'),
                 type='both')
```

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferis/nat/tarball/master),
and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")
devtools::install_github("nat", "jefferis")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.
