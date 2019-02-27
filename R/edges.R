#'Remove cells in the neighbourhood of a boundary cell
#'
#'A boundary is considered as a cell having a \code{edgeVal}) value. If one of the cell in the neighborhood has a edgeVal value, then all the cells inside the neighborhood will be assigned a NA value.
#'
#'@param x A \code{Raster* object}
#'@param w Numeric. Size of the window around each cell (must be an odd number). Window size around each cell.
#'@param edgeVal value of cells that are on the outer side of edges
#'@param filename Character (optional). If the output is written to disk, full path to location including filename
#'@param ... Additional arguments as for \code{\link[raster]{writeRaster}}
#'@export

edges <- function(x,
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



