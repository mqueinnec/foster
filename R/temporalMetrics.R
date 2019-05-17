#' Calculate temporal summary metrics
#'
#' This function calculates a set of user-defined or default statistics from a variable time series. If \code{s} is a Raster object, each layer should be a year of the time series. If \code{s} is a SpatialPointsDataFrame object, each column should be a year of the time series. The argument \code{fun} defines which metrics will be calculated. It has to be the name of a function that takes a vector as input and returns a named vector corresponding to the summary metrics. The function \code{defaultTemporalSummary} is used by default and returns the median, IQR and Theil-Sen slope (calculated with \code{\link[wql]{mannKen}}) of the time series.
#'
#' If \code{s} is a Raster object, the processing can be parallelized using \code{\link[raster]{cluster}}. In that case the user has to set \code{par = TRUE} and provide the number of parallel threads \code{threads}.
#'
#' @param s Input Raster or SpatialPointsDataFrame object containing a time series (may be generated with \code{\link[foster]{calcIndices}})
#' @param metrics Name of a function  used to process the time series provided as a character.
#' @param prefix Optional. Character that will be added to the names of the output layers
#' @param filename Character. Output filename including path to directory and eventually extension
#' @param par Logical. Should the function be executed in parallel threads
#' @param threads Number of parallel threads used if par = T
#' @param m tuning parameter to determine how many blocks will be used (m blocks will be processed by each cluster)
#' @param progress Logical. If TRUE (default) a progress bar is displayed.
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#' @seealso \code{\link[raster]{calc}}, \code{\link[raster]{cluster}}
#' @examples
#'
#' \dontrun{
#' funSummary <- function(x) {
#'   c(
#'     mean = mean(x, na.rm = T),
#'     median = median(x, na.rm = T),
#'     std = std(x, na.rm = T)
#'   )
#' }
#'
#' temporalMetrics(s, metrics = "funSummary")
#' }
#' @export
#' @import data.table

temporalMetrics <- function(s,
                            metrics = "defaultTemporalSummary",
                            prefix = NULL,
                            filename = "",
                            par = F,
                            threads = 2,
                            progress = TRUE,
                            m = 2,
                            ...) {
  if (is.null(prefix)) prefix <- ""

  if(progress) progress = "text" else progres = NULL

  if (!is.character(metrics)) stop("metrics must be a character (function name)")
  if (any(grepl("\\(|\\)", metrics))) stop("metrics must be a character without
                                         brackets ()")

  eval_fun <- character()
  for (f in 1:length(metrics)) {
    eval_fun[f] <- paste0(metrics[f], "(value)")
  }

  eval_fun <- paste(eval_fun, collapse = ",")

  if (class(s)[1] == "SpatialPointsDataFrame") {
    coordnames(s) <- c("x", "y")
    ind_df <- foster::spdf2df(s, xy = T) # xy arg is not necessary for spdf,
    # it adds a column
    ind_dt <- data.table::as.data.table(ind_df)
    # convert to long
    ind_dt_long <- data.table::melt(ind_dt,
      id.vars = c("x", "y"),
      value.name = "value"
    )
    # generate summary
    result <- ind_dt_long[, j = as.list(eval(parse(text = eval_fun))), by = list(x, y)]
    # output should be spatial object
    result <- sp::SpatialPointsDataFrame(
      coords = result[, c("x", "y")],
      data = result[, -c("x", "y")], proj4string = raster::crs(s)
    )
    names(result) <- paste0(prefix, "_", names(result))

    if (filename != "") {
      rgdal::writeOGR(result,
        driver = "ESRI Shapefile",
        layer = tools::file_path_sans_ext(basename(filename)),
        dsn = dirname(filename), ...
      )
    }
  } else {
    if (par) {
      raster::beginCluster(threads)
      result <- raster::clusterR(s,
        fun = calc,
        args = list(fun = eval(parse(text = metrics))),
        filename = filename, m = m, progress = progress,
        ...
      )
      raster::endCluster()
    } else {
      result <- raster::calc(s,
        fun = eval(parse(text = metrics)),
        filename = filename, progress = progress,
        ...
      )
      names(result) <- paste0(prefix, "_", names(result))
    }
  }
  return(result)
}
