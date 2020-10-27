#'Match the resolution of two Raster* objects
#'
#'Successively projects (if necessary) and resamples a raster coordinate system
#'and spatial resolution to the reference
#'
#'\code{x} and \code{ref} must have defined CRS (can be assigned using
#'\code{\link[raster]{projection}}). If the CRS don't match, \code{x} is
#'projected to \code{ref} CRS prior to resampling. \code{x} doesn't inherit the
#'extent of \code{ref}.
#'
#'@param x Raster* object or list of Raster* objects.
#'@param ref Reference Raster* object with parameters that \code{x} should be
#'  resampled to.
#'@param method Character. Method used to compute values for the resampled
#'  raster. Can be \code{'bilinear'} for bilinear interpolation or \code{'ngb'}
#'  for nearest neighbor interpolation. See \code{\link[raster]{resample}}.
#'@param filename Character. Output file name including path to directory and
#'   eventually extension. If \code{x} is a list, \code{filename} must be a vector of characters with one file name for each element of x. Default is \code{""} (output not written to disk).
#'@param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#'@return Raster* object or list of Raster* objects.
#'@seealso \code{\link[raster]{resample}}, \code{\link[raster]{projectRaster}},
#'  \code{\link[raster]{projection}}
#'@examples
#' # Load raster package
#' library(raster)
#'
#' # Open ALS metric and Landsat BAP imagery
#' elev_p95 <- raster(system.file("extdata/examples/ALS_metrics_p95.tif",package="foster"))
#' BAP_2006 <- stack(system.file("extdata/examples/Landsat_BAP_2006.tif",package="foster"))
#'
#' matchResolution(x = elev_p95,ref = BAP_2006,method='bilinear')
#'@export

matchResolution <- function(x,
                            ref,
                            method="bilinear",
                            filename="",
                            ...) {
  UseMethod("matchResolution", x)
}

#'@export
matchResolution.Raster <-  function(x,
                            ref,
                            method="bilinear",
                            filename="",
                            ...){


  if (!class(x)[1] %in% c("RasterLayer", "RasterBrick", "RasterStack")) {
    stop("x must be a Raster object")
  }

  if (!class(ref)[1] %in% c("RasterLayer", "RasterBrick", "RasterStack")) {
    stop("ref must be a Raster object")
  }

  #Check CRS
  if (is.na(raster::crs(x)) | is.na(raster::crs(ref))) {
    stop("CRS of x or ref is not defined")
  } else if (!raster::compareCRS(raster::crs(x), raster::crs(ref))) {
    warning("x and ref don't have the same CRS. x is projected to ref CRS before
            resampling")
    x <- raster::projectRaster(x,
                               crs = raster::crs(ref))
  }

  if (raster::extent(ref) > raster::extent(x)) {
    #We crop ref to x extent. It avoids creating a large resampled x if ref
    #extent is much larger than x
    ref_crop <- raster::crop(ref, x, filename = "")
  } else {
    ref_crop <- ref
  }
  #Resampling

  out <- raster::resample(x = x, y = ref_crop, method = method, filename =
                            filename, ...)

  return(out)
}

#'@export
matchResolution.list <- function(x,
                                 ref,
                                 method="bilinear",
                                 filename="",
                                 ...) {

  if (length(filename) < length(list)) {
    # Append missing filenames
    toAdd <- length(list) - length(filename)
    filename = c(filename, rep("", toAdd))
  }

  # Check if filenames are unique (other than "")
  filename_non_empty <- filename[filename != ""]

  if (length(unique(filename_non_empty)) != length(filename_non_empty)) {
    stop("filename must have unique values (other than \"\") for each element of the list x")
  }

  args <- c(as.list(environment()), list(...))
  args <- args[ ! names(args) %in% c("x", "filename")]
  mapply(matchResolution, x, filename, MoreArgs = args)
  }


