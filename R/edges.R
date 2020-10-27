#' Assign NA values to the neighborhood of a boundary cell
#'
#' Assigns NA value to all cells having a NA values within their \code{w x w}
#' neighborhood.
#'
#' @param x A \code{Raster* object}
#' @param w Numeric. Size of the window around each cell. Must be an odd number.
#' @param filename Character. Output file name including path to directory and
#'   eventually extension. Default is \code{""} (output not written to disk).
#' @param ... Additional arguments passed to \code{\link[raster]{writeRaster}}
#' @seealso \code{\link[raster]{focal}}
#' @return Raster* object
#' @examples
#' # Load raster package
#' library(raster)
#'
#' # Open and stack ALS metrics
#' elev_p95 <- raster(system.file("extdata/examples/ALS_metrics_p95.tif",package="foster"))
#' cover <- raster(system.file("extdata/examples/ALS_metrics_cov_mean.tif",package="foster"))
#' Y_vars <- stack(elev_p95,cover)
#'
#' # Remove edges in a 3 x 3 neighborhood
#' Y_vars_edges <- edges(Y_vars, w=3)
#' @export

edges <- function(x,
                  w,
                  filename = "",
                  ...) {
  if (w %% 2 == 0) stop("w must be an odd number")

  filt <- matrix(0, nrow = w, ncol = w)
  filt[floor(w / 2) + 1, floor(w / 2) + 1] <- 1


  if (class(x)[1] == "RasterLayer") {
    out <- raster::focal(x = x, w = filt, na.rm = FALSE, pad = FALSE, NAonly = FALSE,
                         filename = filename, ...)
  } else if (class(x)[1] %in% c("RasterStack", "RasterBrick")) {
    x.list <- raster::as.list(x)
    out <- lapply(x.list, function(r) raster::focal(x = r, w = filt, na.rm = FALSE,
                                            pad = FALSE, NAonly = FALSE, filename = ""))
    out <- raster::stack(out)

    if (filename != "") {
      raster::writeRaster(out, filename = filename, ...)
    }
  } else {
    stop("x must be a Raster object")
  }

  names(out) <- names(x)

  return(out)
}
