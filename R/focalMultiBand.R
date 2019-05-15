#' Apply a spatial filter to a Raster object
#'
#' Apply a spatial filter to a RasterLayer or all layers of a RasterStack or
#' RasterBrick object. The mathematical operation applied within the
#' neighbourhood can be done by using a function (\code{fun}) or by setting
#' the weights of the matrix \code{w}.
#'
#'
#' If x contains NA values and \code{na.rm = TRUE} is used , using \code{fun} or
#'  \code{w} with weights adjusted to apply equivalent mathematical operation
#'  might not produce the same outputs (in that case using weights would give
#'  wrong results). See the documentation of \code{\link[raster]{focal}} for
#'  more information.
#'  Also, cells of x with NA values might get a non-NA value assigned when
#'  located in the neighbourhood of non-NA cells and \code{na.rm = TRUE} is used.
#'  In that case, setting \code{keepNA = TRUE} ensures that NA cells of x still
#'  have NA values in the output raster.
#'
#' @param x A Raster* object
#' @param w Matrix of weights (moving window). A 3x3 windows with weights of 1
#' would be \code{w=matrix(1,nr=3,nc=3)} for example.
#' @param fun function (optional). The function should accept a vector of values
#'  and return a single number (e.g. mean). It should also accept a
#'  \code{na.rm} argument.
#' @param na.rm logical. If TRUE (default), NA are removed from computation
#' @param pad logical. IF TRUE, rows and columns are added around \code{x} to
#' avoid removing border cells.
#' @param padValue numeric. Value of \code{pad} cells. Usually set to NA and
#' used in combination with \code{na.rm=TRUE}
#' @param NAonly logical. If TRUE only cell values that are NA are replaced with
#'  the computed focal values
#' @param filename Character (optional). Output filename including path to
#' directory and eventually extension
#' @param keepNA Logical. If TRUE (default), NA cells of \code{x} are unchanged
#' @param ... Additional arguments passed to \code{\link[raster]{writeRaster}}
#' @seealso \code{\link[raster]{focal}}
#'
#' @export

focalMultiBand <- function(x,
                           w,
                           fun,
                           filename = "",
                           na.rm = TRUE,
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
  } else if (!class(x[1]) %in% c("RasterStack", "RasterBrick")) {
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
    out <- writeRaster(out, filename = filename, ...)
  }

  names(out) <- names(x)

  return(out)
}
