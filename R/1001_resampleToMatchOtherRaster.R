# resampling rasters to landsat grid

# inputs:
# -raster to resample
# -landsat image
#
# library(raster)
# library(lidR)
# library(UsefulRFunctions)
# library(tidyverse)
#
#
# foster_resample <- function(reference_image,
#                             image_to_resample,
#                             output_dir=NA,
#                             output_name=NA,
#                             output_format="tif",
#                             overwrite=TRUE) {
#   #inputs can be either paths to rasters or raster objects
#   #check if input reference image is raster
#   if(!class(reference_image)[1]== "RasterLayer") reference_image <- raster(reference_image)
#   if(!class(image_to_resample)[1]== "RasterLayer") image_to_resample <- raster(image_to_resample)
#
#   #if output name is not provided, then write the resampled image with modified filename
#   if(is.na(output_name)) {
#     output_name <- create_name(r=image_to_resample, output_format = "tif")
#   }
#
#   #if output_dir is NA then write image in the same dir
#   if(is.na(output_dir))  output_dir <- dirname(image_to_resample@file@name)
#
#   #construct output_image based on output_dir and output_name
#   output_image <- paste0(output_dir,"/", output_name)
#
#   r <- raster::resample(x = image_to_resample,
#                         y = reference_image,
#                         filename=output_image,
#                         method="bilinear",
#                         overwrite=overwrite)
# }

# foster_resample(reference_image = "Z:\\LandsatExtrapolationProject\\NTEMS\\HarrysRiver\\DEM\\UTM21S_DEM.dat",
#                 image_to_resample = "O:\\TEMP\\foster\\ABA_rasters\\HarrysRiver_B_ABA.tif")


# MatchResolution(ref = "Z:\\LandsatExtrapolationProject\\NTEMS\\HarrysRiver\\DEM\\UTM21S_DEM.dat",
#                 x = "O:\\TEMP\\foster\\ABA_rasters\\HarrysRiver_B_ABA.tif")
#

















# # devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
# # # devtools::install_github("Jean-Romain/lidR", dependencies=TRUE, )
# # devtools::install_github("Jean-Romain/lidR", ref="devel",  dependencies=TRUE)
#
# # add missing RTools 3.5 info
# v_i = devtools:::version_info
# v_i[["3.5"]] = v_i[["3.4"]]
# v_i[["3.5"]]$version_max = "3.5.99"
# assignInNamespace(x     = "version_info",
#                   value = v_i,
#                   ns    = "devtools")
#
# # now find_rtools should work properly
# devtools::find_rtools()
