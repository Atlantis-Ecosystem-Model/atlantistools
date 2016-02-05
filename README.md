<!-- README.md is generated from README.Rmd. Please edit that file -->
atlantistools
=============

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/alketh/atlantistools.png?branch=master)](https://travis-ci.org/alketh/atlantistools) [![Build status](https://ci.appveyor.com/api/projects/status/github/alketh/atlantistools?branch=master&svg=true)](https://ci.appveyor.com/project/alketh/atlantistools)

atlantistools is a data processing and visualisation tool for R, which helps to process output from Atlantis models within R. Using atlantistools makes sure that Atlantis users use the same input/output file structure which facilitates intra and inter model comparisons.

Installation
------------

Get the development version from github:

``` r
# install.packages(devtools)
devtools::install_github("alketh/atlantistools")
```

If you want to check out the vignette please use

``` r
devtools::install_github("alketh/atlantistools", build_vignettes = TRUE)
vignette("package-demo", package = "atlantistools")
```
