default_temporal_summary <- function(x) {
  list(
    median=median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}


#'Calculate temporal metrics
#'
#'@param s Input \code{raster::stack} object containing a time series of spectral indices (e.g. NDVI). Input stack may be generated with \code{calcIndices}
#'@param fun Name of a function (e.g. \code{"median"} or \code{"myFunction"}) used to process the time series data. Function should return a named \code{list}. \code{default_temporal_summary} function used as the default returns median, IQR, and slope values.
#'@param output_dir Path to the output directory. If not provided, output is saved in the same directory as \code{x}.
#'@param output_name Filename of the resampled raster without extension. If not provided \code{x} name is used with an appendix (_resampled)
#'@param output_format Extension of the resampled raster. (Default=tif).
#'@param overwrite Logical. Should the output raster be overwritten?

TemporalMetrics <- function(s,
                             fun="default_temporal_summary",
                             output_dir=NA,
                             output_name=NA,
                             output_format="tif",
                             overwrite=T) {
  #input is a raster stack or???
  #user should be able to specify the function
  #currently supporting stack only

  ind.df <- as.data.frame(s, xy=T)
  ind.dt <- as.data.table(ind.df)

  #filter nas
  ind.dt <- ind.dt[!is.na(eval(parse(text=names(ind.dt)[3])))]

  #convert to long
  ind.dt.long <-  melt(ind.dt, id.vars = c("x","y"))

  #generate summary
  result <- ind.dt.long[,j=eval(parse(text=paste0(fun,"(value)"))),by=list(x,y)]
  # result <- ind.dt.long[, j=myFun(value),by = list(x,y)]



  # output should be a raster
  coordinates(result) <- ~x+y
  gridded(result) <- TRUE

  r <- stack(result)


  #output name
  if(is.na(output_name)) {
    output_name <- create_name(s, output_name=output_name, odix = paste0("_",fun ))
  }
  #output dir
  if(is.na(output_dir))  output_dir <- dirname(x@file@name)

  #construct output_image based on output_dir and output_name
  output_image <- paste0(output_dir,"/", output_name)

  writeRaster(r,output_image, overwrite=overwrite, bylayer=T, suffix=names(r))

  # return(r)
}
