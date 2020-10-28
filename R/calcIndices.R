#'Calculate spectral indices from multispectral data
#'
#'Calculate spectral indices (e.g. NDVI, tasseled cap coefficients etc.) from
#'multispectral data. Calculations are based on the functions
#'\code{\link[RStoolbox]{spectralIndices}} and
#'\code{\link[RStoolbox]{tasseledCap}}. Refer to the documentation of these
#'functions for more details.
#'
#'If x is a Raster* or list of Raster* objects, each layer should be one of the
#'spectral bands used to calculate the indices. If x is a SpatialPointsDataFrame
#'or list of spatialPointsDataFrame, each column should be a spectral band. When
#'calculating tasseledCap indices, bands should be provided in a specific order
#'specified in \code{\link[RStoolbox]{tasseledCap}}.
#'
#'Tasseled Cap Angle (TCA) and Distance (TCD) are calculated from greenness
#'(TCG) and brightness (TCB) as follows:
#'
#'\deqn{TCA = \arctan(\frac{TCG}{TCB})}
#'
#'\deqn{TCD = \sqrt{TCB^{2} + TCG^{2}}}
#'
#'If \code{x} is a list of Raster* objects, the processing can be parallelized
#'using \code{\link[raster]{cluster}}. In that case the user has to set
#'\code{par = TRUE} and provide the number of parallel threads \code{threads}.
#'You can control how many blocks will be processed by each thread by setting
#'\code{m} (see \code{\link[raster]{cluster}}).
#'
#'@param x Raster* or SpatialPointsDataFrame object or list of Raster* or
#'  SpatialPointsDataFrame objects.
#'@param indices Character vector indicating Which indices are calculated.
#'  Tasseled Cap indices are abbreviated as \code{TCB}, \code{TCW}, \code{TCG},
#'  \code{TCA}, \code{TCD}. For a list of other supported indices see
#'  \code{\link[RStoolbox]{spectralIndices}}
#'@param sat Character. If calculating tasseled cap indices, name of the sensor
#'  needs to be provided. One of: c("Landsat4TM", "Landsat5TM", "Landsat7ETM",
#'  "Landsat8OLI", "MODIS", "QuickBird", "Spot5", "RapidEye"). See
#'  \code{\link[RStoolbox]{tasseledCap}}.
#'@param blue Integer. Blue band.
#'@param green Integer. Green band.
#'@param red Integer. Red band.
#'@param nir Integer. Near infrared band (700-1100 nm).
#'@param swir1 temporarily deprecated
#'@param swir2 Integer. Shortwave infrared band (1400-1800 nm)
#'@param swir3 Integer. Shortwave infrared band (2000-2500 nm)
#'@param coefs Coefficients necessary to calculate some of the spectral indices
#'  (e.g. EVI). See \code{\link[RStoolbox]{spectralIndices}}.
#'@param filename Character. Output file name including path to directory and
#'  eventually extension. If \code{x} is a list, \code{filename} must be a
#'  vector of characters with one file name for each element of x. Default is
#'  \code{""} (output not written to disk).
#'@param par Logical. Should the function be executed on parallel threads
#'@param threads Number of parallel threads used if par = TRUE
#'@param m tuning parameter to determine how many blocks will be used (m blocks
#'  will be processed by each cluster)
#'@param progress Logical. If TRUE (default) a progress bar is displayed when
#'  using parallel processing.
#'@param ... Other arguments passed to \code{\link[raster]{writeRaster}} or
#'  \code{\link[rgdal]{writeOGR}}.
#'@seealso \code{\link[RStoolbox]{spectralIndices}},
#'  \code{\link[RStoolbox]{tasseledCap}}, \code{\link[raster]{cluster}}
#'@return Raster* or SpatialPointsDataFrame object or list of Raster* or
#'  SpatialPointsDataFrame objects.
#'@examples
#'\donttest{
#' library(raster)
#' 
#' # Open Landsat BAP image
#' BAP_2006 <- stack(system.file("extdata/examples/Landsat_BAP_2006.tif",package =
#'                                 "foster"))
#' 
#' # Calculate NDVI
#' VI_2006 <- calcIndices(BAP_2006,
#'                        indices = "NDVI",
#'                        red=3,
#'                        nir=4)
#'}
#'@export

calcIndices <- function(x,
                        indices = "NDVI",
                        sat = NULL,
                        blue = NULL,
                        green = NULL,
                        red = NULL,
                        nir = NULL,
                        swir1 = NULL,
                        swir2 = NULL,
                        swir3 = NULL,
                        coefs = list(
                          L = 0.5, G = 2.5, L_evi = 1, C1 = 6,
                          C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL
                        ),
                        filename = "",
                        par = FALSE,
                        threads = 2,
                        m = 2,
                        progress = TRUE,
                        ...) {



  if (!is.character(indices)) {
    stop("indices must be a vector of characters")
  }

  UseMethod("calcIndices", x)
}

#' @export
calcIndices.Raster <- function(x,
                               indices = "NDVI",
                               sat = NULL,
                               blue = NULL,
                               green = NULL,
                               red = NULL,
                               nir = NULL,
                               swir1 = NULL,
                               swir2 = NULL,
                               swir3 = NULL,
                               coefs = list(
                                 L = 0.5, G = 2.5, L_evi = 1, C1 = 6,
                                 C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL
                               ),
                               filename = "",
                               par = FALSE,
                               threads = 2,
                               m = 2,
                               progress = TRUE,
                               ...) {
  if (!(class(x) %in% c(
    "SpatialPointsDataFrame", "RaterLayer", "RasterStack",
    "RasterBrick"
  ))) {
    stop("x must be a SpatialPointsDataFrame, RasterLayer, RasterStack or
         RasterBrick object (or a list of these objects)")
  }

  if(progress) progress = "text" else progres = NULL

  # Supported Tasseled Cap names
  TC.names <- c("TCG", "TCB", "TCW", "TCA", "TCD")
  if (any(indices %in% TC.names)) {
    doTC <- TRUE
  }else{
    doTC <- FALSE
  }
  if (doTC && is.null(sat)) {
    stop("The name of the satellite must be given to calculate Tasseled Cap
         indices")
  }

  # Rename x to x_rast for use below
  x_rast <- x

  if (par) {
    raster::beginCluster(threads)

    if (doTC) {
      out_tc <- raster::clusterR(x_rast, RStoolbox::tasseledCap,
                                 args = list(sat = sat), filename = "", m = m, progress = progress, ...
      )
      names(out_tc) <- c("TCB", "TCG", "TCW")
    }

    out_ind <- raster::clusterR(x_rast, RStoolbox::spectralIndices,
                                args = list(
                                  indices = setdiff(indices, TC.names), blue = blue,
                                  green = green, red = red, nir = nir, swir1 = swir1, swir2 = swir2,
                                  swir3 = swir3, coefs = coefs
                                ), filename = "", m = m, progress = progress,
                                ...
    )
    names(out_ind) <- setdiff(indices, TC.names)

    on.exit(raster::endCluster())

    if (doTC) {
      out <- raster::stack(out_tc, out_ind)
    } else {
      out <- out_ind
    }

    if ("TCA" %in% indices) {
      out$TCA <- atan(raster::subset(out, "TCG") /
                        raster::subset(out, "TCB"))
    }
    if ("TCD" %in% indices) {
      out$TCD <- (raster::subset(out, "TCG")^2 +
                    raster::subset(out, "TCB")^2)^0.5
    }

    out <- raster::subset(out, indices)
    temp_names <- names(out)
  } else {
    if (doTC) {
      out_tc <- RStoolbox::tasseledCap(
        img = x_rast, sat = sat, filename = "",
        ...
      )
      names(out_tc) <- c("TCB", "TCG", "TCW")
    }
    out_ind <- RStoolbox::spectralIndices(
      img = x_rast,
      indices = setdiff(indices, TC.names), blue = blue, green = green,
      red = red, nir = nir, swir1 = swir1, swir2 = swir2, swir3 = swir3,
      coefs = coefs, filename = "", ...
    )
    names(out_ind) <- setdiff(indices, TC.names)

    if (doTC) {
      out <- raster::stack(out_tc, out_ind)
    } else {
      out <- out_ind
    }

    if ("TCA" %in% indices) {
      out$TCA <- atan(raster::subset(out, "TCG") /
                        raster::subset(out, "TCB"))
    }
    if ("TCD" %in% indices) {
      out$TCD <- (raster::subset(out, "TCG")^2 +
                    raster::subset(out, "TCB")^2)^0.5
    }

    out <- raster::subset(out, indices)
    temp_names <- names(out)
  }

  if (filename != "") {
    out <- raster::writeRaster(out, filename = filename, ...)
    names(out) <- temp_names
  }

  return(out)
}

#' @export
calcIndices.SpatialPointsDataFrame <- function(x,
                                               indices = "NDVI",
                                               sat = NULL,
                                               blue = NULL,
                                               green = NULL,
                                               red = NULL,
                                               nir = NULL,
                                               swir1 = NULL,
                                               swir2 = NULL,
                                               swir3 = NULL,
                                               coefs = list(
                                                 L = 0.5, G = 2.5, L_evi = 1, C1 = 6,
                                                 C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL
                                               ),
                                               filename = "",
                                               par = FALSE,
                                               threads = 2,
                                               m = 2,
                                               progress = TRUE,
                                               ...) {
  if (!(class(x) %in% c(
    "SpatialPointsDataFrame", "RaterLayer", "RasterStack",
    "RasterBrick"
  ))) {
    stop("x must be a SpatialPointsDataFrame, RaterLayer, RasterStack or
         RasterBrick object")
  }

  if(progress) progress = "text" else progres = NULL

  # Supported Tasseled Cap names
  TC.names <- c("TCG", "TCB", "TCW", "TCA", "TCD")
  if (any(indices %in% TC.names)) doTC <- TRUE else doTC <- FALSE
  if (doTC && is.null(sat)) {
    stop("The name of the satellite must be given to calculate Tasseled Cap
         indices")
  }

  # We transform the spdf to a raster
  x_df <- spdf2df(x, xy = TRUE)
  # Store coordinates of x
  coords <- x_df[, sp::coordnames(x)]
  x_df <- x_df[, !(colnames(x_df) %in% sp::coordnames(x))]
  # Transform x to a raster stack
  x_rast <- raster::stack(lapply(as.list(x_df), function(x) {
  raster::raster(matrix(x))
    }))

  if (doTC) {
    out_tc <- RStoolbox::tasseledCap(
        img = x_rast, sat = sat, filename = "",
        ...
      )
      names(out_tc) <- c("TCB", "TCG", "TCW")
  }
  out_ind <- RStoolbox::spectralIndices(
      img = x_rast,
      indices = setdiff(indices, TC.names), blue = blue, green = green,
      red = red, nir = nir, swir1 = swir1, swir2 = swir2, swir3 = swir3,
      coefs = coefs, filename = "", ...
    )
    names(out_ind) <- setdiff(indices, TC.names)

  if (doTC) {
      out <- raster::stack(out_tc, out_ind)
  } else {
      out <- out_ind
  }

  if ("TCA" %in% indices) {
      out$TCA <- atan(raster::subset(out, "TCG") /  raster::subset(out, "TCB"))
  }

  if ("TCD" %in% indices) {
    out$TCD <- (raster::subset(out, "TCG")^2 + raster::subset(out, "TCB")^2)^0.5
    }
    out <- raster::subset(out, indices)
    temp_names <- names(out)


    # Transform back to spdf
    out <- raster::rasterToPoints(out, spatial = FALSE)
    out <- sp::SpatialPointsDataFrame(
      coords = coords,
      data = data.frame(out[, indices]), proj4string = raster::crs(x)
    )
    names(out) <- temp_names

    if (filename != "") {
      rgdal::writeOGR(out,
                      driver = "ESRI Shapefile",
                      layer = tools::file_path_sans_ext(basename(filename)),
                      dsn = dirname(filename), ...
      )
    }

  return(out)
}

#' @export
calcIndices.list <- function(x,
                             indices = "NDVI",
                             sat = NULL,
                             blue = NULL,
                             green = NULL,
                             red = NULL,
                             nir = NULL,
                             swir1 = NULL,
                             swir2 = NULL,
                             swir3 = NULL,
                             coefs = list(
                               L = 0.5, G = 2.5, L_evi = 1, C1 = 6,
                               C2 = 7.5, s = 1, swir2ccc = NULL, swir2coc = NULL
                             ),
                             filename = "",
                             par = FALSE,
                             threads = 2,
                             m = 2,
                             progress = TRUE,
                             ...) {

  # Check that there is a unique filename for each element of the list
  if (length(filename) < length(x)) {
    if (filename != "") {
      warning("Number of filenames and processed objects don't match. Datasets with missing filename will be stored in memory or written to a temporary file")
    }
    # Append missing filenames
    toAdd <- length(x) - length(filename)
    filename = c(filename, rep("", toAdd))
  }

  # Check if filenames are unique (other than "")
  filename_non_empty <- filename[filename != ""]

  if (length(unique(filename_non_empty)) != length(filename_non_empty)) {
    stop("filename must have unique values (other than \"\") for each element of the list x")
  }

  args <- c(as.list(environment()), list(...))
  args <- args[ ! names(args) %in% c("x", "filename")]
  mapply(calcIndices, x, filename, MoreArgs = args)

  }

