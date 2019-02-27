#'Remove edges of input Raster
#'
#'@param x A \code{Raster* object}
#'@param w Numeric. Window size around each cell. If one of the cell in the neighborhood has a edgeVal value, then all the cells inside the neighborhood will be removed.
#'@param edgeVal value of cells that are on the outer side of edges
#'@param filename Character (optional). If the output is written to disk, full path to location including filename
#'@param ... Additional arguments as for \code{\link[raster]{writeRaster}}
#'@export

edgesFoster <- function(x,
                        w,
                        filename='',
                        edgeVal = NA,
                        ...) {

  if(w%%2==0) stop("w must be an odd number")

  filt <- matrix(0,nr=w,nc=w)
  filt[floor(w/2)+1,floor(w/2)+1] = 1

  if(class(x)[1] == 'RasterLayer'){
    out <- raster::focal(x=x,w=filt,na.rm=F,pad=F,NAonly=F,filename=filename,...)
  }else if(!class(x[1]) %in% c('RasterStack','RasterBrick')){
    x.list <- raster::as.list(x)
    out <- lapply(x.list,function(r) raster::focal(x=r,w=filt,na.rm=F,pad = F,NAonly=F,filename=''))
    out <- raster::stack(out)

    if(filename != ''){
      writeRaster(out,filename = filename, ...)
    }

  }else{
    stop("x must be a Raster object")
  }

  names(out) <- names(x)



  return(out)




}



