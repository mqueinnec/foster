#'Calculate median, IQR and Theil Sen slope
#'
#'This function is usually called within \code{\link[foster]{temporalMetrics}}
#'
#'@param x Vector of numeric values
#'@return A list with median, IQR and slope
#'@export
defaultTemporalSummary <- function(x) {
  c(
    mean=mean(x,na.rm=T),
    sd=sd(x,na.rm=T),
    median=median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}


#'Convert a SPDF to data.frame
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


unwrap_indices <- function(x,y){
  #x is a list
  #y is a stacked index dataset, at one timestep

  for (n in 1:length(names(y))){
    if(class(y)[1]=="SpatialPointsDataFrame"){
      x[[names(y)[n]]] <- c(x[[names(y)[n]]],y[names(y)[n]])
    }else{
      x[[names(y)[n]]] <- c(x[[names(y)[n]]],raster::subset(y,names(y)[n]))
    }
  }

  if(class(y)[1]=="SpatialPointsDataFrame"){
    x <- lapply(x, function(x) do.call(cbind,x))
  }else{
    x <- lapply(x,function(x) raster::stack(x))
  }
  return(x)
}




