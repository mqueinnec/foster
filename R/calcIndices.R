#' calcIndices
#'
#' Calculates spectral indices (e.g. NDVI, tasseled cap coefficients etc.) from multispectral images. Calculations are based on the functions \code{\link[RStoolbox]{spectralIndices}} and \code{\link[RStoolbox]{tasseledCap}}. Refer to the documentation of these functions for more details.
#'
#' If x is a raster, each layer should be one of the spectral bands used to calculate the indices. If x is a spatialPointsDataFrame, each column should be a spectral band. When calculating tasseledCap indices, bands should be provided in a specific order specified in \code{\link[RStoolbox]{tasseledCap}}.
#' When using parallel processing, you can control how many blocks will be processed by each cluster by setting \code{m}.
#'
#' @param x Raster or spatialPointsDataFrame object composed of the multispectral bands used to calculate the indices
#' @param indices Character vector indicating Which indices are calculated. Tasseled Cap indices are abbreviated as \code{TCB}, \code{TCW}, \code{TCG}, \code{TCA}, \code{TCD}. For a list of other supported indices see \code{\link[RStoolbox]{spectralIndices}}
#' @param sat Character. If calculating tasseled cap indices, name of the sensor needs to be provided. One of: c("Landsat4TM", "Landsat5TM", "Landsat7ETM", "Landsat8OLI", "MODIS", "QuickBird", "Spot5", "RapidEye").
#' @param blue Integer. Blue band.
#' @param green Integer. Green band.
#' @param red Integer. Red band.
#' @param nir Integer. Near infrared band (700-1100nm).
#' @param swir1 temporarily deprecated
#' @param swir2 Integer. Shortwave infrared band (1400-1800 nm)
#' @param swir3 Integer. Shortwave infrared band (2000-2500 nm)
#' @param coefs Coefficients necessary to calculate some of the spectral indices (e.g. EVI)
#' @param filename Character. Output filename including path to directory and eventually extension
#' @param par Logical. Should the function be executed on parallel threads
#' @param threads Number of parallel threads used if par = TRUE
#' @param m tuning parameter to determine how many blocks will be used (m blocks will be processed by each cluster)
#' @param progress Logical. If TRUE (default) a progress bar is displayed when using parallel processing.
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#' @seealso \code{\link[RStoolbox]{spectralIndices}}, \code{\link[RStoolbox]{tasseledCap}}, \code{\link[raster]{cluster}}
#' @return A Raster object with spectral indices
#' @export

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
                        par = F,
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

  if (class(x) == "SpatialPointsDataFrame") { # We transform the spdf to a raster
    x_df <- spdf2df(x, xy = T)
    # Store coordinates of x
    coords <- x_df[, sp::coordnames(x)]
    x_df <- x_df[, !(colnames(x_df) %in% sp::coordnames(x))]
    # Transform x to a raster stack
    x_rast <- raster::stack(lapply(as.list(x_df), function(x) {
      raster(matrix(x))
    }))
  } else {
    x_rast <- x
  }

  if (par && class(x) != "SpatialPointsDataFrame") {
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

    on.exit(endCluster())

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

  if (class(x) == "SpatialPointsDataFrame") {
    # Transform back to spdf
    out <- raster::rasterToPoints(out, spatial = F)
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
  } else if (filename != "") {
    out <- writeRaster(out, filename = filename, ...)
    names(out) <- temp_names
  }

  return(out)
}
