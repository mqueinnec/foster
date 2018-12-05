#'Crop and mask input raster to match the extent of the reference
#'
#'@param x Data to crop and mask. May be a \code{raster} object or a path to a raster file.
#'@param ref Reference raster. May be a \code{raster} object or a path to a raster file.
#'@param filename Character. Output filename without directory. Can contain file extension (supported by writeRaster)
#'@param outdir Character. Output directory. If not provided will be written in the same directory as \code{x}. If \code{x}
#'is in memory, will be written in working directory
#'@param ... Other arguments passed to \code{writeRaster} (e.g. format, overwrite ...)

MatchExtent <- function(x,
                        ref,
                        filename='',
                        outdir=NA,
                        ...) {

  filename <- trimws(filename)

  #inputs can be either paths to rasters or raster objects
  #check if input reference image is raster
  if(!(class(ref)[1]== "RasterLayer"|class(ref)[1]== "RasterStack"|class(ref)[1]== "RasterBrick")) ref <- raster::raster(ref)
  if(!(class(x)[1]== "RasterLayer"|class(x)[1]== "RasterStack"|class(x)[1]== "RasterBrick")) x <- raster::stack(x)

  if(is.na(crs(x))){
    warning("x doesn't have any specified CRS")
  }else if(!compare(crs(x),crs(ref))){
    warning("x and ref don't have the same CRS")
  }

  out <- raster::crop(x,ref)

  if (filename==''){
    out <- raster::mask(out,ref)
  }else{
    if(is.na(outdir) & !raster::filename(x)=="") outdir <- dirname(x@file@name)
    if(is.na(outdir) & raster::filename(x)=="") outdir <- getwd()
    filename <- file.path(outdir,filename)
    out <- raster::mask(out,ref,filename=filename)
  }

  return(out)
}
