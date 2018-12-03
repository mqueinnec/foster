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
                        sat=NULL,
                        coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5,
                                     s = 1, swir2ccc = NULL, swir2coc = NULL),
                        ...) {

  #Spectral Indices functions (from RSToolbox)
  .IDXdb <-  list(
    CTVI    = function(red, nir) {(NDVI+.5)/sqrt(abs(NDVI+.5))},
    DVI     = function(red, nir) {s*nir-red},
    EVI      = function(red, nir, blue) {G * ((nir - red) / (nir + C1 * red - C2 * blue + L_evi))},
    EVI2    = function(red, nir) {G * (nir-red)/(nir + 2.4*red +1)},
    GEMI    = function(red, nir) {(((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5) ) / (nir + red + 0.5)) * 0.25)) - ((red - 0.125) / (1 - red))},
    GNDVI    = function(green, nir) {(nir-green)/(nir+green)},
    MNDWI   = function(green, swir2) {(green-swir2) / (green+swir2)},
    MSAVI    = function(red, nir) {nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red))))},
    MSAVI2    = function(red, nir) {(2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2},
    NBRI    = function(nir, swir3) { (nir - swir3) / (nir + swir3)},
    NDVI    = function(red, nir) {(nir-red)/(nir+red)},
    NDVIC   = function(red, nir, swir2) {(nir-red)/(nir+red)*(1-((swir2 - swir2ccc)/(swir2coc-swir2ccc)))},
    NDWI     = function(green, nir) {(green - nir)/(green + nir)},
    NDWI2    = function(nir, swir2) {(nir - swir2)/(nir + swir2)},
    NRVI    = function(red, nir) {(red/nir - 1)/(red/nir + 1)},
    RVI     = function(red, nir) {red/nir},
    SATVI   = function(red, swir2, swir3) {(swir2 - red) / (swir2 + red + L) * (1 + L) - (swir3 / 2)},
    SAVI    = function(red, nir) {(nir - red) * (1+L) / (nir + red + L)},
    SLAVI    = function(red, nir, swir2) {nir / (red + swir2)},
    SR      = function(red, nir) {nir / red},
    TVI     = function(red, nir) {sqrt((nir-red)/(nir+red)+0.5)},
    TTVI    = function(red, nir) {sqrt(abs((nir-red)/(nir+red) + 0.5))},
    WDVI    = function(red, nir) {nir - s * red}
  )

  BANDSdb <- lapply(.IDXdb, function(x) names(formals(x)))

  #Tasseled Cap indices (from RSToolbox)

  d <- list(NULL, c("brightness", "greenness", "wetness"))
  .TCcoefs <- list(
    landsat4tm = matrix(c(
      # Crist 1985
      0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303,
      -0.1603, -0.2819, -0.4934, 0.7940, 0.0002, -0.1446, #typo 0.1063, typo -0.0002
      0.0315,  0.2021,  0.3102, 0.1594, 0.6806, -0.6109), #type -0.6806
      ncol=3, dimnames = d),
    landsat5tm = matrix( c(
      # Crist 1985
      0.2043,  0.4158,  0.5524, 0.5741,  0.3124,  0.2303,
      -0.1603, -0.2819, -0.4934, 0.7940, 0.0002, -0.1446,
      0.0315,  0.2021,  0.3102, 0.1594, 0.6806, -0.6109)
      , ncol = 3, dimnames = d),
    landsat7etm= matrix(c(
      # Huang 2002
      0.3561,  0.3972, 0.3904, 0.6966, 0.2286, 0.1596,
      -0.3344, -0.3544,-0.4556, 0.6966,-0.0242,-0.2630,
      0.2626,  0.2141, 0.0926, 0.0656,-0.7629,-0.5388)
      #       0.0805, -0.0498 ,0.1950,-0.1327, 0.5752,-0.7775,
      #      -0.7252, -0.0202, 0.6683, 0.0631,-0.1494,-0.0274,
      #       0.4000, -0.8172, 0.3832, 0.0602,-0.1095, 0.0985
      , ncol = 3, dimnames = d),
    landsat8oli= matrix(c(
      # Baig et al (2014)
      0.3029, 0.2786, 0.4733, 0.5599, 0.5080, 0.1872,
      -0.2941,-0.2430,-0.5424, 0.7276, 0.0713,-0.1608,
      0.1511, 0.1973, 0.3283, 0.3407,-0.7117,-0.4559), ncol = 3, dimnames = d),
    modis = matrix(c(
      #Lobser & Cohen (2007)
      0.4395, 0.5945, 0.2460, 0.3918, 0.3506, 0.2136, 0.2678,
      -0.4064, 0.5129,-0.2744,-0.2893, 0.4882,-0.0036,-0.4169,
      0.1147, 0.2489, 0.2408, 0.3132,-0.3122,-0.6416,-0.5087), ncol = 3, dimnames = d),
    quickbird = matrix(c(
      #Yarbrough et al. (2005)
      0.319, -0.121, 0.652, 0.677,
      0.542, -0.331, 0.375, -0.675,
      0.490, -0.517, -0.639, 0.292), ncol = 3, dimnames = d),
    spot5 = matrix(c(
      #Ivtis et al. (2008)
      0.492, 0.610, 0.416, 0.462,
      -0.196, -0.389, 0.896, -0.084,
      0.397, 0.260, 0.118, -0.872), ncol = 3, dimnames = d),
    rapideye = matrix(c(
      #Schoenert et al. (2014)
      0.2435, 0.3448, 0.4881, 0.4930, 0.5835,
      -0.2216, -0.2319, -0.4622, -0.2154, 0.7981,
      -0.7564, -0.3916, 0.5049, 0.1400, 0.0064), ncol = 3, dimnames = d)
    #ikonos = matrix(c(NA)),
  )

  #other_args <- list(...)

  #Check if x has another class than supported ones
  #if(!class(x)[1] == "RasterBrick" || !class(x)[1] == "RasterStack") x <- raster::stack(x)
  out <- x
  out_temp <- list()
  #if (raster::canProcessInMemory(out,3)) { #Can be processed in RAM
  for (m in 1:length(method)){
    if (method[m] == "TC"){
      sat <- tolower(sat)
      if(class(x)[1]=="SpatialPointsDataFrame") {
        cof <- .TCcoefs[[sat]]
        out_temp[[m]] <- as.matrix(x@data) %*% cof
        out_temp[[m]] <- cbind(out_temp[[m]],
                               tca=atan(out_temp[[m]][,"greenness"]/out_temp[[m]][,"brightness"]),
                               tcd=((out_temp[[m]][,"greenness"])^2+(out_temp[[m]][,"brightness"])^2)^0.5)
      }else{
        out_temp[[m]] <- RStoolbox::tasseledCap(x, sat, ...)
        tca <- atan(out_temp[[m]]$greenness/out_temp[[m]]$brightness)
        names(tca) <- "TCA"
        tcd <- (out_temp[[m]]$greenness^2+out_temp[[m]]$brightness^2)^0.5
        names(tcd) <- "TCD"
        out_temp[[m]] <- raster::stack(out_temp[[m]],tca,tcd)
      }
    }else{
      ind <- toupper(method[m])
      if(class(x)[1] =="SpatialPointsDataFrame") {
        cbands <- list(...)[names(list(...)) %in% BANDSdb[[ind]]]
        bands <- lapply(cbands,function(c) x@data[,c])
        L = coefs[["L"]]
        G = coefs[["G"]]
        L_evi = coefs[["L_evi"]]
        C1 = coefs[["C1"]]
        C2 = coefs[["C2"]]
        s = coefs[["s"]]
        swir2ccc = coefs[["swir2ccc"]]
        swir2cdiff = coefs[["swir2cdiff"]]
        out_temp[[m]] <- data.frame(do.call(.IDXdb[[ind]],bands))
        colnames(out_temp[[m]]) <- ind
      }else{
        out_temp[[m]] <- RStoolbox::spectralIndices(x,indices=method[m], ...)
      }
    }
  }

  #Combine indices of multiple methods
  if(class(x)[1]=="SpatialPointsDataFrame") {
    all_ind <- do.call(cbind,out_temp)
    out <- sp::SpatialPointsDataFrame(coordinates(x),proj4string=crs(x),data=data.frame(all_ind))
  }else{
    out <- do.call(raster::stack,out_temp)
  }
  #}else{ #Cannot be processed in RAM
  #Processing with chunks
  #}

  #Calculate indices
  if(filename != "") {
    out <- raster::writeRaster(out,filename=filename,...)
  }

  return(out)

}


