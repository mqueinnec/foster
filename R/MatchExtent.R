#'Match the extent of a reference Raster
#'
#'Crops or extends the extent of a raster to the extent of a reference. Some cells of the reference raster can optionally be masked based on their values.
#'
#'\code{x} and \code{ref} need to have the same CRS, spatial resolution and origin. If this is not the case, you can use \code{\link[foster]{MatchResolution}}.
#'
#'@param x Raster* object
#'@param ref Raster* object with extent that \code{x} shoudl be cropped to
#'@param mask logical. Should x be masked by \code{ref} cells that have the value \code{maskValue}
#'@param inverse logical. If TRUE, cells of \code{ref} that are not \code{maskvalue} are masked
#'@param maskValue Value of \code{ref} cells that should be masked in \code{x}
#'@param filename Character. Output filename
#'@param ... Other arguments passed to \code{writeRaster}
#'@return A \code{Raster*} object
#'@export

matchExtent <- function(x,
                        ref,
                        mask=F,
                        inverse=FALSE,
                        maskValue=NA,
                        filename='',
                        ...) {

  if(!any(class(x)[1] %in% c('RasterLayer','RasterStack','RasterBrick'))) {
    stop("x must be a Raster object")
  }

  if(!any(class(ref)[1] %in% c('RasterLayer','RasterStack','RasterBrick'))) {
    stop("ref must be a Raster object")
  }

  if(!raster::compareCRS(x,ref) | !all(raster::origin(x)==raster::origin(ref)) | !all(raster::res(x)==raster::res(ref))) {
    stop("x and ref don't have the same CRS, origin or spatial resolution. Consider resampling before using MatchExtent")
  }

  extent_x <- raster::extent(x)
  extent_ref <- raster::extent(ref)
  area_x <- (extent_x@xmax - extent_x@xmin)*(extent_x@ymax-extent_x@ymin)
  area_ref <- (extent_ref@xmax - extent_ref@xmin)*(extent_ref@ymax-extent_ref@ymin)

  if(area_x<area_ref){
    x_extent <- raster::extend(x,ref[[1]],filename='')
  }else if(raster::extent(x)>raster::extent(ref)){
    x_extent <- raster::crop(x,ref[[1]],filename='')
  }else{
    x_extent <- x
  }
  names(x_extent) <- names(x)

  if(mask){
    out <- raster::mask(x=x_extent,mask=ref[[1]],inverse=inverse,maskValue=maskValue,filename=filename,...)
  }else{
    out <- x_extent
    if (filename != ""){
      out <- writeRaster(out,filename=filename,...)
    }
  }

  return(out)
}
