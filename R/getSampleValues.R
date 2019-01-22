#'Get raster cell values at samples
#'
#'@param x A raster layer
#'@param s Locations of the samples. Object of class \code{SpatialPointsDataFrame}, usually generated with \code{getSampleXY}
#'@return A \code{SpatialPointsDataFrame} object containing pixel values corresponding to sample locations

getSampleValues <- function(x, s) {

  if(!any(class(x)[1] %in% c('RasterLayer','RasterBrick','RasterStack'))){
    stop("x must be a Raster object")
  }
  if(class(s) != "SpatialPointsDataFrame"){
    stop("s must be a SpatialPointsDataFrame object")
  }

  values <- as.data.frame(x[raster::cellFromXY(x, s)])
  values <- sp::SpatialPointsDataFrame(coords = coordinates(s), proj4string=crs(x),data=values)
  names(values) <- names(x)
  return(values)

}
