#'Match the extent of a reference Raster
#'
#'Crop and mask input raster to match the extent of the reference
#'
#'\code{x} and \code{ref} need to have the same CRS, spatial resolution and origin. If this is not the case, you can use \code{\link[foster]{MatchResolution}}. For more information about cropping and masking function you can refer to \code{\link[raster]{crop}} and \code{\link[raster]{mask}}
#'
#'@param x Raster* object
#'@param ref Raster* object with extent that \code{x} shoudl be cropped to
#'@param mask logical. Should x be masked by \code{ref} cells that have the value \code{maskValue}
#'@param maskValue logical. Value of \code{ref} cells that should be masked in \code{x}
#'@param filename Character. Output filename
#'@param ... Other arguments passed to \code{writeRaster}
#'@return A \code{Raster*} object

matchExtent <- function(x,
                        ref,
                        mask=F,
                        inverse=FALSE,
                        maskValue=NA,
                        updatevalue=NA,
                        updateNA = FALSE,
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

  extent.x <- raster::extent(x)
  extent.ref <- raster::extent(ref)
  area.x <- (extent.x@xmax - extent.x@xmin)*(extent.x@ymax-extent.x@ymin)
  area.ref <- (extent.ref@xmax - extent.ref@xmin)*(extent.ref@ymax-extent.ref@ymin)

  if(area.x<area.ref){
    x.extent <- raster::extend(x,ref[[1]],filename='')
  }else if(raster::extent(x)>raster::extent(ref)){
    x.extent <- raster::crop(x,ref[[1]],filename='')
  }else{
    x.extent <- x
  }
  names(x.extent) <- names(x)

  if(mask){
    out <- raster::mask(x=x.extent,mask=ref[[1]],inverse=inverse,maskValue=maskValue,updatevalue=updatevalue,updateNA=updateNA,filename=filename,...)
  }else{
    out <- x.extent
    if (filename != ""){
      out <- writeRaster(out,filename=filename,...)
    }
  }

  return(out)
}
