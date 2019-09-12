foster: Forest Structure Extrapolation with R
======================================================================================================
<img src="https://raw.githubusercontent.com/samherniman/foster/master/man/figures/thick_border-equal_flight-lower.png" align="right"/>

This package contains functions that can be used to extrapolate sparse forest attribute data on larger scale using a k-NN imputation approach. 

FOSTER allows to perform the following tasks: 

* Data preprocessing: Resampling, cropping, masking, smoothing raster datasets
* Calculate spectral indices and summarize time-series of variables to be used as predictors
* Select training and testing samples using a stratified random selection approach
* Train a k-NN model and assess its accuracy 
* Impute response variables simultaneously on a large scale using the trained k-NN model 

## Installation 
```
install.packages("remotes")
remotes::install_github("mqueinnec/foster")
library(foster)
```

## Vignette
We advise reading through the vignette to get familiar with FOSTER. It contains a short description of the package and a step-by-step example to illustrate the framework around which it has been designed. 

By default, the vignette is not built when installing the package. In order to access the vignette there are two solutions: 

* Force building vignettes using ```devtools::install_github("mqueinnec/foster", build_vignettes = TRUE)```. Note that the package might take a few minutes to install since the vignette needs to be created by R. The vignette can then be accessed with ```browseVignette("foster")```. 
* You can aslo directly download the html vignette located in the doc folder of this repository. However, it won't be built when installing the package and therefore ```browseVignette("foster")``` will indicate that the vignette doesn't exist. 

