#' Match the extent of a reference raster
#'
#' This function crops or extends the extent of a raster to the extent of a
#' reference. Some cells of the reference raster can optionally be masked based
#' on their values.
#'
#' \code{x} and \code{ref} need to have the same CRS, spatial resolution and
#' origin. If this is not the case, you can use
#' \code{\link[foster]{matchResolution}} before \code{matchExtent}.
#'
#' @param x Raster* object or list of Raster* objects.
#' @param ref Raster* object. \code{x} extent will be matched to \code{ref}
#'   extent.
#' @param mask Logical. Should x be masked by \code{ref} cells that have the
#'   value \code{maskValue}
#' @param inverse Logical. If TRUE, cells of \code{ref} that are not
#'   \code{maskvalue} are masked
#' @param maskValue Value of \code{ref} cells that should be masked in \code{x}.
#'   Default is \code{NA}.
#' @param filename Character. Output file name including path to directory and
#'   eventually extension. If \code{x} is a list, \code{filename} must be a vector of characters with one file name for each element of x. Default is \code{""} (output not written to disk).
#' @param ... Other arguments passed to \code{writeRaster}
#' @return Raster* object or list of Raster* objects.
#' @seealso \code{\link[raster]{crop}}, \code{\link[raster]{extend}},
#'   \code{\link[raster]{mask}}
#' @examples
#' # Load raster package
#' library(raster)
#'
#' # Open ALS p95 and mask of forested areas as Raster objects
#' BAP_2006 <- stack(system.file("extdata/examples/Landsat_BAP_2006.tif",package="foster"))
#' mask_forest <- raster(system.file("extdata/examples/VLCE_forest_2008.tif",package="foster"))
#'
#' matchExtent(BAP_2006, mask_forest, mask = TRUE)
#' @export

matchExtent <- function(x,
                        ref,
                        mask=FALSE,
                        inverse=FALSE,
                        maskValue=NA,
                        filename="",
                        ...) {

  UseMethod("matchExtent", x)
}

#'@export
matchExtent.Raster <- function(x,
                        ref,
                        mask=FALSE,
                        inverse=FALSE,
                        maskValue=NA,
                        filename="",
                        ...) {

  if (!any(class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
    stop("x must be a Raster object")
  }

  if (!any(class(ref)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
    stop("ref must be a Raster object")
  }

  if (!raster::compareCRS(x, ref) | !all(raster::origin(x) ==
      raster::origin(ref)) | !all(raster::res(x) == raster::res(ref))) {
    stop("x and ref don't have the same CRS, origin or spatial resolution.
         Consider projecting or resampling before using MatchExtent")
  }

  extent_x <- raster::extent(x)
  extent_ref <- raster::extent(ref)
  area_x <- (extent_x@xmax - extent_x@xmin) * (extent_x@ymax - extent_x@ymin)
  area_ref <- (extent_ref@xmax - extent_ref@xmin) *
    (extent_ref@ymax - extent_ref@ymin)

  if (area_x < area_ref) {
    x_extent <- raster::extend(x, ref[[1]], filename = "")
  } else if (raster::extent(x) > raster::extent(ref)) {
    x_extent <- raster::crop(x, ref[[1]], filename = "")
  } else {
    x_extent <- x
  }
  names(x_extent) <- names(x)

  if (mask) {
    out <- raster::mask(x = x_extent, mask = ref[[1]], inverse = inverse,
                        maskValue = maskValue, filename = filename, ...)
  } else {
    out <- x_extent
    if (filename != "") {
      out <- raster::writeRaster(out, filename = filename, ...)
      names(out) <- names(x)
    }
  }
  return(out)
}

#'@export
matchExtent.list <- function(x,
                             ref,
                             mask=FALSE,
                             inverse=FALSE,
                             maskValue=NA,
                             filename="",
                             ...) {

  if (length(filename) < length(list)) {
    # Append missing filenames
    toAdd <- length(list) - length(filename)
    filename = c(filename, rep("", toAdd))
  }

  # Check if filenames are unique (other than "")
  if (length(unique(filename[filename != ""])) != length(filename[filename != ""])) {
    stop("filename must have unique values (other than \"\") for each element of the list x")
  }

  args <- c(as.list(environment()), list(...))
  args <- args[ ! names(args) %in% c("x", "filename")]
  mapply(matchExtent, x, filename, MoreArgs = args)

  }
