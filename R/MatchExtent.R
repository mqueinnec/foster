#'Crop and mask input raster to match the extent of the reference
#'
#'@param x Data to crop and mask. May be a \code{raster} object or a path to a raster file.
#'@param ref Reference raster. May be a \code{raster} object or a path to a raster file.
#'@param mask logical. Should x be masked by \code{ref} cells that have the value \code{maskValue}
#'@param maskValue logical. Value of \code{ref} cells that should be masked in \code{x}
#'@param filename Character. Output filename without directory. Can contain file extension (supported by writeRaster)
#'is in memory, will be written in working directory
#'@param ... Other arguments passed to \code{writeRaster} (e.g. format, overwrite ...)

matchExtent <- function(x,
                        ref,
                        mask=T,
                        maskValue=NA,
                        filename='',
                        ...) {

  filename <- trimws(filename)

  if(!raster::compareCRS(x,ref) | !all(raster::origin(x)==raster::origin(ref)) | !all(raster::res(x)==raster::res(ref))) {
    stop("x and ref don't have the same CRS, origin or spatial resolution. Consider resampling before using MatchExtent")
  }

  if(mask){
    x.crop <- raster::crop(x,ref,filename='')
  }else{
    x.crop <- x
  }

  out <- raster::mask(x.crop,ref,maskValue=maskValue,filename=filename)
  names(out) <- names(x)

  return(out)
}
