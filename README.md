
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FOSTER: Forest structure extrapolation with R

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/mqueinnec/foster.svg?branch=master)](https://travis-ci.com/mqueinnec/foster)
<!-- badges: end -->

The goal of foster is to streamline the modeling of the relationship
between satellite imagery time series or any other environmental
information, such as terrain elevation, with forest structural
attributes derived from 3D point cloud data and their subsequent
imputation over the broader landscape.

The package is organized around functions for data preprocessing,
stratified random sample selection, spectral index calculation, time
series summary metrics calculation, k-NN predictive model development
and their accuracy assessment, and finally response variable
(i.e. forest attributes) imputation. The following figure shows a
diagram explaining the general processing flow a user might encounter
when imputing forest attributes from selected predictors that may
include time series of multispectral satellite images and topographic
variables.

![FOSTER
workflow](https://raw.githubusercontent.com/mqueinnec/storage/main/FOSTER/FOSTER_workflow.png)

## Installation

You can install the development version of foster from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mqueinnec/foster")
```

## Get Started

A vignette is available to give an overview of FOSTER and run through an
example of ALS metrics imputation.

``` r
browseVignettes("foster")
```

Note that if using the development version of FOSTER, the vignette to be
build during installation to access it with `browseVignette("foster")`.
It will take a few minutes to build the vignette.

``` r
devtools::install_github("mqueinnec/foster", build_vignettes = TRUE)
```
