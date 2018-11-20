#'Crop and mask input raster to match the extent of the reference
#'
#'@param x Data to crop and mask. May be a \code{raster} object or a path to a raster file.
#'@param ref Reference raster. May be a \code{raster} object or a path to a raster file.
#'@param output_dir Path to the output directory. If not provided, output is saved in the same directory as \code{x}.
#'@param output_name Filename of the resampled raster without extension. If not provided \code{x} name is used with an appendix (_resampled)
#'@param output_format Extension of the resampled raster. (Default=tif).
#'@param overwrite Logical. Should the output raster be overwritten?




MatchExtent <- function(x,
                        ref,
                        output_dir=NA,
                        output_name=NA,
                        output_format="tif",
                        overwrite=TRUE) {

  #check if input reference image is raster or rasterstack
  if(!class(ref)[1]== "RasterLayer") ref <- raster(ref)

  #if x is not a rasterlayer or stack the read it from disk as stack
  if(!class(x)[1]== "RasterLayer" || !class(x)[1]== "RasterStack") x <- stack(x)


  out <- crop(x, ref) #crop
  out <- mask(out, ref)       #mask

  #write output
  #if output name is not provided, then write the resampled image with modified filename
  if(is.na(output_name)) {
    output_name <- create_name(r=x, output_format = "tif",odix="_crop")
  }

  #if output_dir is NA then write image in the same dir
  if(is.na(output_dir)) {
    if(class(r)[1]== "RasterLayer") output_dir <- dirname(x@file@name)
    if(class(r)[1]== "RasterStack") output_dir <- dirname(x@layers[[1]]@file@name)
  }

  #construct output_image based on output_dir and output_name
  output_image <- paste0(output_dir,"/", output_name)

  writeRaster(out,output_image, overwrite=overwrite)
}
