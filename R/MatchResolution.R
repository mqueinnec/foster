#'Resample a raster from a reference raster
#'
#'@param x Data to resample. May be an existing \code{raster} object or a path to a raster file.
#'@param ref Reference raster (e.g. wall-to-wall satellite image). \code{x} will be resampled to match \code{ref}. May be an existing \code{raster} object or a path to a raster file.
#'@param method Character. Method used to compute values for the resampled raster. Can be \code{'bilinear'} for bilinear
#'interpolation or \code{'ngb'} for nearest neighbor interpolation.
#'@param filename Character. Output filename without directory. Can contain file extension (supported by writeRaster)
#'@param outdir Character. Output directory. If not provided will be written in the same directory as \code{x}. If \code{x}
#'is in memory, will be written in working directory
#'@param ... Other arguments passed to \code{writeRaster} (e.g. format, overwrite ...)

MatchResolution <- function(x,
                               ref,
                               method='bilinear',
                               filename='',
                               outdir=NA,
                               ...){

  filename <- trimws(filename)

  #inputs can be either paths to rasters or raster objects
  #check if input reference image is raster
  if(!(class(ref)[1]== "RasterLayer"|class(ref)[1]== "RasterStack"|class(ref)[1]== "RasterBrick")) ref <- raster::raster(ref)
  if(!(class(x)[1]== "RasterLayer"|class(x)[1]== "RasterStack"|class(x)[1]== "RasterBrick")) x <- raster::raster(x)

  #Check if x and ref have the same coordinate system
  if(!raster::compareCRS(crs(x),crs(ref))){
    warning("x and ref don't have the same CRS. x is projected to ref CRS before resampling")
    x <- raster::projectRaster(x,crs(ref))
  }

  #If a filename is provided, write the output to file on disk
  #If it is not, it tries to process it in memory.
  #If the output is too big to be processed in memory, it'll be written to file anyway (in rasterTmpFile())
  if(filename==''){
    #We don't give filename argument, it will try to process in memory
    out <- raster::resample(x=x,y=ref,method=method,...)
  }else{
    #Check ourdir argument. If NA, set to the basename of x. If x is not from a file, outDir set to getwd()
    if(is.na(outdir) & !raster::filename(x)=="") outdir <- dirname(x@file@name)
    if(is.na(outdir) & raster::filename(x)=="") outdir <- getwd()
    filename <- file.path(outdir,filename)
    out <- raster::resample(x=x,y=ref,method=method,filename=filename,...)
  }
  return(out)
}
