#' Split a raster into tiles
#'
#' This function is used to split a raster into smaller tiles. The raster is
#' split in a grid pattern with \code{nx} columns and \code{ny} rows.
#'
#' @param x Raster* object to split
#' @param nx Number of horizontal cells in the splitting grid
#' @param ny Number of vertical cells in the splitting grid
#' @param filename Character. Output file name including path to directory and
#'   eventually extension.Default is \code{""} (output not written to disk).
#' @param suffix Character appended to filename to differentiate tiles
#' (must have length nx x ny). If left NULL, tiles will be numbered by columns
#' and rows
#' @param ... Additional parameters passed to \code{\link[raster]{writeRaster}}
#' @seealso \code{\link[raster]{crop}}
#' @return A list of Raster* objects
#' @examples
#' \dontrun{
#' # X_vars is a RasterStack where each layer is a predictor variable
#'
#' # Split X_vars into a 5 x 5 grid
#' tile(X_vars, nx = 5, ny = 5)
#' }
#' @export

tile <- function(x,
                 nx,
                 ny,
                 filename = "",
                 suffix = NULL,
                 ...) {
  if (filename != "") {
    if (is.null(suffix)) {
      suffix <- paste0("_", as.character(seq(1, nx * ny)))
    } else if (length(suffix) != nx * ny) {
      stop("Suffix must have the length nx*ny")
    } else {
      suffix <- as.character(suffix)
    }
    filenames <- paste0(tools::file_path_sans_ext(filename), suffix)
    if (tools::file_ext(filename) != "") {
      filenames <- paste0(filenames, ".", tools::file_ext(filename))
    }
  } else {
    filenames <- rep("", times = nx * ny)
  }

  nr <- raster::nrow(x)
  nc <- raster::ncol(x)

  inc_c <- floor(nc / nx)
  inc_r <- floor(nr / ny)

  e <- list()
  n <- 1
  c1 <- 0
  for (i in 1:nx) {
    c0 <- c1 + 1
    r1 <- 0
    for (j in 1:ny) {
      r0 <- r1 + 1
      r1 <- j * inc_r
      c1 <- i * inc_c
      if (i == nx) c1 <- nc
      if (j == ny) r1 <- nr
      e[[n]] <- raster::extent(x, r0, r1, c0, c1)
      n <- n + 1
    }
  }

  result <- list()
  for (i in 1:length(e)) {
    result[[i]] <- raster::crop(x, e[[i]], filenames[[i]], ...)
  }

  return(result)
}
