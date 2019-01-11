#'Resample a raster from a reference raster
#'
#'@param x \code{Raster*} object to resample
#'@param ref Reference \code{Raster*} object with parameters that \code{x} should be resampled to.
#'@param method Character. Method used to compute values for the resampled raster. Can be \code{'bilinear'} for bilinear
#'interpolation or \code{'ngb'} for nearest neighbor interpolation.
#'@param filename Character. Output filename without directory. Can contain file extension (supported by writeRaster)
#'is in memory, will be written in working directory
#'@param ... Other arguments passed to \code{writeRaster} (e.g. format, overwrite ...)

matchResolution <- function(x,
                               ref,
                               method='bilinear',
                               filename='',
                               ...){

  filename <- trimws(filename)

  if(!class(x)[1] %in% c('RasterLayer','RasterBrick','RasterStack')){
    stop("x must be a Raster object")
  }

  if(!class(ref)[1] %in% c('RasterLayer','RasterBrick','RasterStack')){
    stop("ref must be a Raster object")
  }

  #Check CRS
  if(is.na(crs(x)) | is.na(crs(ref))){
    stop("CRS of x or ref is not defined")
  }else if(!raster::compareCRS(crs(x),crs(ref))){
    warning("x and ref don't have the same CRS. x is projected to ref CRS before resampling")
    x <- raster::projectRaster(x,crs(ref))
  }

  #We crop ref to x extent. It avoids creating a large resampled x if ref extent is much larger than xs
  ref_crop <- raster::crop(ref,x,filename='')

  #Resampling
  out <- raster::resample(x=x,y=ref_crop,method=method,filename=filename,...)

  return(out)
}
