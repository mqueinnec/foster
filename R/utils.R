#'Calculate median, IQR and Theil Sen slope
#'
#'This function is usually called within \code{\link[foster]{temporalMetrics}}
#'
#'@param x Vector of numeric values
#'@return A list with median, IQR and slope
#'
#'
defaultTemporalSummary <- function(x) {
  list(
    mean=mean(x,na.rm=T),
    sd=sd(x,na.rm=T),
    median=median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}

defaultTemporalSummary_v2 <- function(x) {
  c(
    mean=mean(x,na.rm=T),
    sd=sd(x,na.rm=T),
    median=median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}

remove_extension <- function(x){ #copied from tools::file_path_sans_ext
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

create_name <- function(r,output_format="tif",odix="_resampled") {
  if(class(r)[1]== "RasterLayer"|| class(r)[1]== "RasterBrick") input_basename <- remove_extension(basename(r@file@name))
  if(class(r)[1]== "RasterStack") input_basename <- remove_extension(basename(r@layers[[1]]@file@name))
  paste0(input_basename, odix, ".",output_format)

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




