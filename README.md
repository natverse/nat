# nat

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Released versions
The recommendation is to install from our lab repository:

```r
install.packages('frulhns',repos=c(getOption("repos"),'http://jefferislab.org/R'),
                 type='both')
```

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferis/nat/tarball/master), and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("nat", "jefferis")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.
