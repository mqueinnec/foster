#'@title calcIndices
#'
#'@description Calculates spectral indices from multibands spectral images
#'
#'@param x Composite raster composed of the spectral bands used to calculate the indices. X can be a path to raster or RasterBrick object
#'@param method Which method is used to calculate the indices. Default is Tasseled Cap. FOr a list of other methods see RStoolbox::spectralIndices
#'@param filename character. Optional output filename. If no filename is provided, the output is written to disk only if they cannot be stored in RAM (raster::canProcessinMemory)
#'@param ... Further arguments passed to RStoolbox:tasseledCap, RStoolbox:spectralIndices or raster::writeRaster
#'
#'@return A Raster object with spectral indices


calcIndices <- function(x,
                        method="TC",
                        filename = '',
                        ...) {

  #other_args <- list(...)

  #Check if x has another class than supported ones
  if(!class(x)[1] == "RasterBrick" || !class(x)[1] == "RasterStack") x <- raster::stack(x)

  out <- x
  out_temp <- list()
  #if (raster::canProcessInMemory(out,3)) { #Can be processed in RAM
  for (m in 1:length(method)){
    if (method[m] == "TC"){
      out_temp[[m]] <- RStoolbox::tasseledCap(x, ...)
    }else{
      out_temp[[m]] <- RStoolbox::spectralIndices(x,indices=method[m], ...)
    }
  }
  out <- do.call(raster::stack,out_temp)
  #}else{ #Cannot be processed in RAM
  #Processing with chunks
  #}

  #Calculate indices
  if(filename != "") {
    out <- raster::writeRaster(out,filename=filename,...)
  }

  return(out)
}
