#'Get pixel values for sample locations
#'
#'@param x A raster layer
#'@param s Locations of the samples. Object of class \code{SpatialPointsDataFrame}. Can be generated with \code{getSampleXY}
#'@return A \code{SpatialPointsDataFrame} object containing pixel values corresponding to sample locations

GetSampleValues <- function(x, s) {


  values <- as.data.frame(x[cellFromXY(x, s)])
  values <- SpatialPointsDataFrame(coords = coordinates(s), data=values)
  return(values)

}
