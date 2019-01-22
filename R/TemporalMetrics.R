#'Calculate temporal metrics
#'
#'@param x Input \code{raster::stack} object containing a time series of spectral indices (e.g. NDVI). Input stack may be generated with \code{calcIndices}
#'@param fun Name of a function (e.g. \code{"median"} or \code{"myFunction"}) used to process the time series data. Function should return a named \code{list}. \code{default_temporal_summary} function used as the default returns median, IQR, and slope values.
#'@param output_dir Path to the output directory. If not provided, output is saved in the same directory as \code{x}.
#'@param output_name Filename of the resampled raster without extension. If not provided \code{x} name is used with an appendix (_resampled)
#'@param output_format Extension of the resampled raster. (Default=tif).
#'@param overwrite Logical. Should the output raster be overwritten?

temporalMetrics <- function(s,
                            metrics="defaultTemporalSummary",
                            prefix=NULL,
                            filename='',
                            ...) {

  if(is.null(prefix)) prefix <- names(s)[1]

  if(!is.character(metrics)) stop("metrics must be a character (function name)")
  if(any(grepl("\\(|\\)",metrics))) stop("metrics must be a character without brackets ()")

  eval.fun = character()
  for(f in 1:length(metrics)){
    eval.fun[f] <- paste0(metrics[f],"(value)")
  }

  eval.fun <- paste(eval.fun,collapse=",")

  #currently supporting stack only
  if (class(s)[1]=="SpatialPointsDataFrame") {
    ind.df <- raster::as.data.frame(s) #xy arg is not necessary for spdf, it adds a column
  }else{
    ind.df <- raster::as.data.frame(s, xy=T)
  }
  ind.dt <- data.table::as.data.table(ind.df)

  #convert to long
  ind.dt.long <-  data.table::melt(ind.dt, id.vars = c("x","y"),value.name='value')

  #filter NAs
  #ind.dt.long <- ind.dt.long[!is.na(ind.dt.long$value),]

  #generate summary
  result <- ind.dt.long[,j=eval(parse(text=eval.fun)),by=list(x,y)]
  #result <- eval(parse(text=paste0("ind.dt.long[,j=list(",eval.fun,"),by=list(x,y)]")))
  # result <- ind.dt.long[, j=myFun(value),by = list(x,y)]

  # output should be spatial object
  coordinates(result) <- ~x+y
  names(result) <- paste0(prefix,"_",names(result))
  #Check the class of the input and if a filename was provided to determine
  #if the output should be a raster or a spatial point
  if (class(s)[1] == "SpatialPointsDataFrame"){
    r <- result
    raster::crs(r) <- raster::crs(s)
  }else{
    gridded(result) <- TRUE
    r <- stack(result)
    raster::crs(r) <- raster::crs(s)
  }


  #Setting back the CRS from the input (lost when computing metrics with fun)
  raster::crs(r) <- raster::crs(s)
  out <- r
  if(filename != "") {
    out <- raster::writeRaster(r,filename=filename,...) ##bylayer=T, suffix=names(r)
    names(out) <- names(r)
  }

  return(out)
}
