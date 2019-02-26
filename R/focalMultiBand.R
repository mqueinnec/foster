#'Apply a spatial filter to a Raster object
#'
#'Apply a spatial filter to a RatsterLayer or all layers of a RatserStack or RasterBrick object. The mathematical operation applied within the neighbourhood can be done by using a function (\code{fun}) or by adjusting the weights of the matrix \code{w}.
#'
#'If the Raster contains NA values, applying a function \code{fun} with \code{na.rm=TRUE} or adjusting the weights of \code{w} are not equivalent (in that case the result would be wrong). During filtering, NA cells of \code{x} can get a value assigned if \code{na.rm=T}. In order to keep these cells value to NA, the argument \code{keepNA=TRUE} can be used (default).
#'
#'@param x A Raster* object
#'@param w Matrix of weights (moving window). A 3x3 windows with weights of 1 would be \code{w=matrix(1,nr=3,nc=3)} for example.
#'@param fun function (optional). The function should accept a vector of values and return a single number (e.g. mean). It should also accept a \code{na.rm} argument.
#'@param na.rm logical. If TRUE (default), NA are removed from computation
#'@param pad logical. IF TRUE, rows and columns are added around \code{x} to avoid removing border cells.
#'@param padValue numeric. Value of \code{pad} cells. Usually set to NA and used in combination with \code{na.rm=TRUE}
#'@param NAonly logical. If TRUE only cell values that are NA are replaced with the computed focal values
#'@param filename Character (optional). If the output is written to disk, full path to location including filename
#'@param keepNA Logical. If TRUE (default), NA cells of \code{x} are unchanged
#'@param ... Additional arguments as for \code{\link[raster]{writeRaster}}
#'

focalMultiBand <- function(x,
                        w,
                        fun,
                        filename='',
                        na.rm=TRUE,
                        pad=FALSE,
                        padValue=NA,
                        NAonly=FALSE,
                        keepNA=T,
                        ...) {

  if(class(x)[1] == 'RasterLayer'){
    #Call raster::focal and eventually write to file
    if(missing(fun)){
      out <- raster::focal(x=x,w=w,filename='',na.rm=na.rm,pad=pad,padValue=padValue,NAonly=NAonly,...)
    }else{
      out <- raster::focal(x=x,w=w,fun=fun,filename='',na.rm=na.rm,pad=pad,padValue=padValue,NAonly=NAonly,...)
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

  }else{
    stop("x must be a Raster object")
  }

  #Do we want to keep NA values from x?
  if(keepNA){
    out <- raster::mask(x=out,mask=x,maskValue=NA)
  }

  #Write to file if filename provided
  if(filename != ''){
    writeRaster(out,filename = filename, ...)
  }

  names(out) <- names(x)

  return(out)




}



