#'Resample lidar-based rasters to match satellite images
#'
#'@param x Data to resample. May be an existing \code{raster} object or a path to a raster file.
#'@param ref Reference raster (e.g. wall-to-wall satellite image). \code{x} will be resampled to match \code{ref}. May be an existing \code{raster} object or a path to a raster file.
#'@param output_dir Path to the output directory. If not provided, output is saved in the same directory as \code{x}.
#'@param output_name Filename of the resampled raster without extension. If not provided \code{x} name is used with an appendix (_resampled)
#'@param output_format Extension of the resampled raster. (Default=tif).
#'@param overwrite Logical. Should the output raster be overwritten? (Default=TRUE)



MatchResolution <- function(x, #image to resample
                     ref, # reference
                     output_dir=NA,
                     output_name=NA,
                     output_format="tif",
                     overwrite=TRUE) {

  # ?assertive::is_class()

  #inputs can be either paths to rasters or raster objects
  #check if input reference image is raster
  if(!class(ref)[1]== "RasterLayer") ref <- raster(ref)
  if(!class(x)[1]== "RasterLayer") x <- raster(x)

  #if output name is not provided, then write the resampled image with modified filename
  if(is.na(output_name)) {
    output_name <- create_name(r=x, output_format = "tif")
  }

  #if output_dir is NA then write image in the same dir
  if(is.na(output_dir))  output_dir <- dirname(x@file@name)

  #construct output_image based on output_dir and output_name
  output_image <- paste0(output_dir,"/", output_name)

  r <- raster::resample(x = x,
                        y = ref,
                        filename=output_image,
                        method="bilinear",
                        overwrite=overwrite)
}
