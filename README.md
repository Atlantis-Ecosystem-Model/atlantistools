<!-- README.md is generated from README.Rmd. Please edit that file -->
atlantistools
=============

[![Project Status](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/alketh/atlantistools.png?branch=master)](https://travis-ci.org/alketh/atlantistools) [![Build status](https://ci.appveyor.com/api/projects/status/github/alketh/atlantistools?branch=master&svg=true)](https://ci.appveyor.com/project/alketh/atlantistools) [![codecov](https://img.shields.io/codecov/c/github/alketh/atlantistools.svg)](https://codecov.io/github/alketh/atlantistools)

`atlantistools` is a data processing and visualisation tool for R, which helps to process output from Atlantis models within R. Using atlantistools makes sure that Atlantis users use the same input/output file structure which facilitates intra and inter model comparisons.

Installation
============

Get the development version from github:

``` r
# install.packages(devtools)
devtools::install_github("alketh/atlantistools")
```

It is highly recommended to install the package with the vignettes:

``` r
devtools::install_github("alketh/atlantistools", build_vignettes = TRUE)
vignette("package-demo", package = "atlantistools")
```

You can customise the build in vignettes to automate the simulate model - check output cycle during model calibration. Currently there are 3 vignettes available.

1.  model-calibration.Rmd
2.  model-calibration-species.Rmd
3.  model-comparison.Rmd

In order to use the vignettes please make sure to use the latest version of RStudio (<https://www.rstudio.com/products/RStudio/>). In addition you need to install pandoc (<http://pandoc.org/installing.html>) and LaTex (I recommend MikTex, <http://miktex.org/download>) on your system.
