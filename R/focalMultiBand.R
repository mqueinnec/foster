#' Apply a spatial filter to a Raster* object
#'
#' Apply a spatial filter to a RasterLayer or all layers of a RasterStack or
#' RasterBrick object. The mathematical operation applied within the
#' neighborhood can be done by using a function (\code{fun}) or by setting
#' the weights of the matrix \code{w}.
#'
#'
#' If x contains NA values and \code{na.rm = TRUE} is used , using \code{fun} or
#'  \code{w} with weights adjusted to apply equivalent mathematical operation
#'  might not produce the same outputs (in that case using weights would give
#'  wrong results). See the documentation of \code{\link[raster]{focal}} for
#'  more information.
#'
#'  Also, cells of x with NA values might get a non-NA value assigned when
#'  located in the neighborhood of non-NA cells and \code{na.rm = TRUE} is used.
#'  In that case, setting \code{keepNA = TRUE} (default) ensures that NA cells of x still
#'  have NA values in the output raster.
#'
#' @param x Raster* object or list of Raster* objects.
#' @param w Matrix of weights (moving window). A 3x3 windows with weights of 1
#' would be \code{w=matrix(1,nr=3,nc=3)} for example.
#' @param fun Function (optional). The function should accept a vector of values
#'  and return a single number (e.g. mean). It should also accept a
#'  \code{na.rm} argument.
#' @param na.rm Logical. If TRUE (default), NAs are removed from computation
#' @param pad Logical. IF TRUE, rows and columns are added around \code{x} to
#' avoid removing border cells.
#' @param padValue Numeric. Value of \code{pad} cells. Usually set to NA and
#' used in combination with \code{na.rm=TRUE}
#' @param keepNA Logical. If TRUE (default), NA cells of \code{x} are unchanged
#' @param NAonly Logical. If TRUE only cell values that are NA are replaced with
#'  the computed focal values.
#' @param filename Character. Output file name including path to directory and
#'   eventually extension. If \code{x} is a list, \code{filename} must be a vector of characters with one file name for each element of x. Default is \code{""} (output not written to disk).
#' @param ... Additional arguments passed to \code{\link[raster]{writeRaster}}
#' @return Raster* object or list of Raster* objects.
#' @seealso \code{\link[raster]{focal}}
#' @examples
#' # Load raster package
#' library(raster)
#'
#' # Open and stack ALS metrics
#' elev_p95 <- raster(system.file("extdata/examples/ALS_metrics_p95.tif",package="foster"))
#' cover <- raster(system.file("extdata/examples/ALS_metrics_cov_mean.tif",package="foster"))
#' Y_vars <- stack(elev_p95,cover)
#'
#' #Define 3x3 filter with weights of 1
#' filt <- matrix(1, nrow = 3, ncol = 3)
#'
#' # Smoothing
#' Y_vars_smooth <- focalMultiBand(x = Y_vars,
#'                                 w=filt,
#'                                 fun=mean,
#'                                 pad=TRUE,
#'                                 padValue=NA,
#'                                 na.rm=TRUE,
#'                                 keepNA = TRUE)
#' @export

focalMultiBand <- function(x,
                           w,
                           fun,
                           filename = "",
                           na.rm = FALSE,
                           pad = FALSE,
                           padValue = NA,
                           NAonly = FALSE,
                           keepNA = TRUE,
                           ...) {
  UseMethod("focalMultiBand", x)
}

#'@export
focalMultiBand.Raster <- function(x,
                           w,
                           fun,
                           filename = "",
                           na.rm = FALSE,
                           pad = FALSE,
                           padValue = NA,
                           NAonly = FALSE,
                           keepNA = TRUE,
                           ...) {
  if (class(x)[1] == "RasterLayer") {
    # Call raster::focal and eventually write to file
    if (missing(fun)) {
      out <- raster::focal(x = x, w = w, filename = "", na.rm = na.rm, pad = pad
                           , padValue = padValue, NAonly = NAonly, ...)
    } else {
      out <- raster::focal(x = x, w = w, fun = fun, filename = "", na.rm = na.rm
                        , pad = pad, padValue = padValue, NAonly = NAonly, ...)
    }
  } else if (class(x)[1] %in% c("RasterStack", "RasterBrick")) {
    # Tranform to list of RasterLayers
    x.list <- raster::as.list(x)
    # Call raster::focal on each layer / no write to filename, except if too
    #large to process in RAM (we wamt to write stack raster layers)
    if (missing(fun)) {
      out <- lapply(x.list, function(r) raster::focal(x = r, w = w, na.rm =
        na.rm, pad = pad, padValue = padValue, NAonly = NAonly, filename = ""))
    } else {
      out <- lapply(x.list, function(r) raster::focal(x = r, w = w, fun = fun,
        na.rm = na.rm, pad = pad, padValue = padValue, NAonly = NAonly, filename
        = ""))
    }
    # Stack the smoothed layers
    out <- raster::stack(out)
  } else {
    stop("x must be a Raster object")
  }

  # Do we want to keep NA values from x?
  if (keepNA) {
    out <- raster::mask(x = out, mask = x, maskValue = NA)
  }

  # Write to file if filename provided
  if (filename != "") {
    names(out) <- names(x)
    out <- raster::writeRaster(out, filename = filename, ...)
  }

  names(out) <- names(x)

  return(out)
}

#'@export
focalMultiBand.list <- function(x,
                                  w,
                                  fun,
                                  filename = "",
                                  na.rm = FALSE,
                                  pad = FALSE,
                                  padValue = NA,
                                  NAonly = FALSE,
                                  keepNA = TRUE,
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
  mapply(focalMultiBand, x, filename, MoreArgs = args)
}
