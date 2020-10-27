#' Extract raster values at sample points
#'
#' Given a Raster* object and a SpatialPointsDataFrame object, the functions returns a SpatialPointsDataFrame objects with the values of the raster at sample points.
#'
#' @param x A Raster* object
#' @param s Location of the sample points. Object of class \code{SpatialPointsDataFrame} generated with \code{\link[foster]{getSample}}
#' @param keepCols Should the columns of \code{s} be retained? Default is FALSE
#' @param filename Character. Output filename including path to directory. File will be automatically saved as an ESRI Shapefile and any extension in \code{filename} will be overwritten
#' @param ... Additional arguments passed to \code{\link[rgdal]{writeOGR}}
#' @return SpatialPointsDataFrame object
#' @seealso \code{\link[raster]{extract}}
#' @examples
#' # Load raster package
#' library(raster)
#'
#' # Open and stack ALS metrics
#' elev_p95 <- raster(system.file("extdata/examples/ALS_metrics_p95.tif",package="foster"))
#' cover <- raster(system.file("extdata/examples/ALS_metrics_cov_mean.tif",package="foster"))
#' Y_vars <- stack(elev_p95,cover)
#' names(Y_vars) <- c("p95","cover")
#'
#' # sample_points is a SpatialPointsDataFrame calculated and saved from getSample
#' # Load it into memory
#' load(system.file("extdata/examples/sample_points.RData",package="foster"))
#'
#' getSampleValues(Y_vars, sample_points)
#' @export

getSampleValues <- function(x,
                            s,
                            keepCols = FALSE,
                            filename = "",
                            ...) {
  if (!any(class(x)[1] %in% c("RasterLayer", "RasterBrick", "RasterStack"))) {
    stop("x must be a Raster object")
  }
  if (class(s) != "SpatialPointsDataFrame") {
    stop("s must be a SpatialPointsDataFrame object")
  }

  values <- raster::extract(x, s, method = "simple", sp = TRUE)

  if (!keepCols) {
    values <- values[, setdiff(names(values), names(s))]
  }

  if (filename != "") {
    rgdal::writeOGR(values, driver = "ESRI Shapefile",
                    layer = tools::file_path_sans_ext(basename(filename)),
                    dsn = dirname(filename), ...)
  }
  return(values)
}
