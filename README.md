[![DOI](https://zenodo.org/badge/3328506.svg)](https://zenodo.org/badge/latestdoi/3328506)


Quantifying the Detection of Early Warning Signals
==================================================

This package contains the source code, analysis, and history of my research published in the Proceedings of the Royal Society Interface,
[_Limits to the Detection of Early Warning Signals for Critical Transitions_](http://dx.doi.org/10.1098/rsif.2012.0125) with Alan Hastings, May 2012. A preprint under CC-by license is freely [available from my website](http://carlboettiger.info/vita.html)

- Author: Carl Boettiger
- License: [CC0](http://creativecommons.org/publicdomain/zero/1.0/)
- [Project navigation](http://carlboettiger.info/2012/05/06/research-workflow.html)


# Installation

Software for the analysis is provided in R package format, see the functions in the `R/` directory. 
These functions can be made avialable by installing the package in R, along with its dependencies.

```r
library(devtools)
install_github("earlywarning", "cboettig")
``` 

The individual-based simulations occassionally used in the analysis require a seperate package,

```r
install_github("populationdynamics", "cboettig")
```

# Use 

Examples for executing the analysis provided in the manuscript are found in `inst/examples`.  Additional examples and some further exploration are found in `inst/examples/appendices`.

# Data

Data files for the analyses contained in the publication are included as R objects as part of the package, see the `data` directory.   
