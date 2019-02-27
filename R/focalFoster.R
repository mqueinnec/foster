#'Apply a filter to image
#'
#'@param x A \code{Raster* object}
#'@param w Matrix of weights
#'@param fun function (optional)
#'@param na.rm logical. If TRUE, NA are removed from computation
#'@param pad logical. IF TRUE
#'@param padValue numeric
#'@param NAonly logical. If TRUE only cel values that are NA are replaced with the computed focal values
#'@param filename Character (optional). If the output is written to disk, full path to location including filename
#'@param ... Additional arguments as for \code{\link[raster]{writeRaster}}
#'@export

focalFoster <- function(x,
                         w,
                         fun,
                         filename='',
                         na.rm=FALSE,
                         pad=FALSE,
                         padValue=NA,
                         NAonly=FALSE,
                         ...) {

  if(class(x)[1] == 'RasterLayer'){
    #Call raster::focal and eventually write to file
    if(missing(fun)){
      out <- raster::focal(x=x,w=w,filename=filename,na.rm=na.rm,pad=pad,padValue=padValue,NAonly=NAonly,...)
    }else{
      out <- raster::focal(x=x,w=w,fun=fun,filename=filename,na.rm=na.rm,pad=pad,padValue=padValue,NAonly=NAonly,...)
    }
  }else if(!class(x[1]) %in% c('RasterStack','RasterBrick')){
    #Tranform to list of RasterLayers
    x.list <- raster::as.list(x)
    #Call raster::focal on each layer / no write to filename, except if too large to process in RAM (we wamt to write stack raster layers)
    if(missing(fun)){
      out <- lapply(x.list,function(r) raster::focal(x=r,w=w,na.rm=na.rm,pad = pad,padValue = padValue,NAonly=NAonly,filename=''))
    }else{
      out <- lapply(x.list,function(r) raster::focal(x=r,w=w,fun=fun,na.rm=na.rm,pad = pad,padValue = padValue,NAonly=NAonly,filename=''))
    }
    #Stack the smoothed layers
    out <- raster::stack(out)

    #Write to file if filename provided
    if(filename != ''){
      writeRaster(out,filename = filename, ...)
    }

  }else{
    stop("x must be a Raster object")
  }

  names(out) <- names(x)



  return(out)




}



