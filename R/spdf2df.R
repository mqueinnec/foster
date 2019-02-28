#'Convert a SPDF to data.frame
#'
#'Convert a SpatialPointsDataFrame to data.frame. Coordinates of \code{x} can be added to the data.frame by using \code{xy=T}
#'
#'@param x A \code{SpatailPointsDataFrame} object
#'@param xy Logical. If TRUE, coordinates of \code{x} are added to data.frame
#'@export

spdf2df <- function(x,xy=F){
  out <- raster::as.data.frame(x)
  if(!xy){
    coord.name <- sp::coordnames(x)
    out[,coord.name[1]] <- NULL
    out[,coord.name[2]] <- NULL
  }
  return(out)
}
