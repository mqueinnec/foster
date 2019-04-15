#'Get raster cell values at samples
#'
#'Uses \code{\link[raster]{extract}} to get values of a Raster object at samples
#'
#'@param x A raster layer
#'@param s Locations of the samples. Object of class \code{SpatialPointsDataFrame}, usually generated with \code{\link[foster]{getSample}}
#'@param append Should the values be appended to s? Default is FALSE
#'@param filename Character. Ouput filename (without extension)
#'@param ... Additional arguments passed to \code{\link[rgdal]{writeOGR}}
#'@return A \code{\link[sp]{SpatialPointsDataFrame}} object containing cells values corresponding to samples
#'@export

getSampleValues <- function(x, s,append=FALSE, filename='', ...) {

  if(!any(class(x)[1] %in% c('RasterLayer','RasterBrick','RasterStack'))){
    stop("x must be a Raster object")
  }
  if(class(s) != "SpatialPointsDataFrame"){
    stop("s must be a SpatialPointsDataFrame object")
  }

  values <- raster::extract(x,s,method='simple',sp=T)

  if(!append){
    values <- values[,setdiff(names(values),names(s))]
  }

  if (filename != ''){
    rgdal::writeOGR(values,driver="ESRI Shapefile",layer=tools::file_path_sans_ext(basename(filename)),dsn=dirname(filename),...)
  }

  return(values)

}
