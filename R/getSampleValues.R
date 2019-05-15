#' Get raster cell values at sample points
#'
#' Given a raster layer and a SpatialPointsDataFrame object, the functions returns a SpatialPointsDataFrame objects with the values of the raster at sample points.
#'
#' @param x A raster layer
#' @param s Location of the sample points. Object of class \code{SpatialPointsDataFrame}, usually generated with \code{\link[foster]{getSample}}
#' @param append Should the values be appended to s? Default is FALSE
#' @param filename Character. Output filename including path to directory. File will be automatically saved as an ESRI Shapefile and any extension in \code{filename} will be overwritten
#' @param ... Additional arguments passed to \code{\link[rgdal]{writeOGR}}
#' @return A \code{\link[sp]{SpatialPoints}} object containing cells values corresponding to samples
#' @seealso \code{\link[raster]{extract}}
#' @export

getSampleValues <- function(x, s, append = FALSE, filename = "", ...) {
  if (!any(class(x)[1] %in% c("RasterLayer", "RasterBrick", "RasterStack"))) {
    stop("x must be a Raster object")
  }
  if (class(s) != "SpatialPointsDataFrame") {
    stop("s must be a SpatialPointsDataFrame object")
  }

  values <- raster::extract(x, s, method = "simple", sp = T)

  if (!append) {
    values <- values[, setdiff(names(values), names(s))]
  }

  if (filename != "") {
    rgdal::writeOGR(values, driver = "ESRI Shapefile",
                    layer = tools::file_path_sans_ext(basename(filename)),
                    dsn = dirname(filename), ...)
  }
  return(values)
}
