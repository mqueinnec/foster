#' Calculate temporal summary metrics
#'
#' This function calculates a set of user-defined or default statistics from spectral indices time series.
#'
#' Spectral indices can be calculated with \code{\link[foster]{calcIndices}}.
#' The input to \code{TemporalMetrics} is a list where each element is a Raster* or a SpatialPointsDataFrame object with layers or columns being spectral indices. Each element should be one step in the time series and elements should be ordered in the time series ascending order.

#' The argument \code{fun} defines which metrics will be calculated. It has to be the name of a function that takes a vector as input and returns a named vector corresponding to the summary metrics. The function \code{defaultTemporalSummary} is used by default and returns the median, IQR and Theil-Sen slope of the time series.
#'
#' If \code{x} is a list of Raster* objects, the processing can be parallelized using \code{\link[raster]{cluster}}. In that case the user has to set \code{par = TRUE} and provide the number of parallel threads \code{threads}. You can control how many blocks will be processed by each thread by setting \code{m} (see \code{\link[raster]{cluster}}).
#'
#' @param x List of Raster* or SpatialPointsDataFrame objects.Input Raster or SpatialPointsDataFrame object containing a time series (may be generated with \code{\link[foster]{calcIndices}})
#' @param metrics Name of a function  used to process the time series provided as a character.
#' @param filename Character. Single output filename including path to directory and eventually extension. Each spectral index is written separately and the name of the spectral index is automatically appended to the file name.
#' @param stack Logical. Should the output be returned as a single RasterStack (TRUE) or as a list containing one Raster per vegetation index (FALSE)
#' @param par Logical. Should the function be executed in parallel threads
#' @param threads Number of parallel threads used if par = TRUE
#' @param m tuning parameter to determine how many blocks will be used (m blocks will be processed by each cluster)
#' @param progress Logical. If TRUE (default) a progress bar is displayed.
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}} or \code{\link[rgdal]{writeOGR}}.
#' @seealso \code{\link[raster]{calc}}, \code{\link[raster]{cluster}}
#' @examples
#' # VI_ts is a list of Raster* calculated and saved from calcIndices
#' # Load it into memory
#' load(system.file("extdata/examples/VI_ts.RData",package="foster"))
#'
#' temporalMetrics(VI_ts, metrics = "defaultTemporalSummary")
#'
#' # User-defined temporal summary metrics can also be used
#' funSummary <- function(x) {
#'   c(
#'     mean = mean(x, na.rm = TRUE),
#'     median = median(x, na.rm = TRUE),
#'     std = sd(x, na.rm = TRUE)
#'   )
#' }
#' @export
#' @import data.table

temporalMetrics <- function(x,
                            metrics = "defaultTemporalSummary",
                            filename = "",
                            stack = TRUE,
                            par = FALSE,
                            threads = 2,
                            progress = TRUE,
                            m = 2,
                            ...) {

  if(!is.list(x)) {
    stop("x must be a list of RasterStack or SpatialPointsDataFrame")
  }

  if(!any(lapply(x, class) %in% c("RasterStack", "RasterBrick", "RasterLayer", "SpatialPointsDataFrame"))) {
    stop("x must be a list of RasterStack or SpatialPointsDataFrame")
  }

  list_class <- lapply(x, class)
  if(!isTRUE(do.call(all.equal, list_class))){
    stop("All elements of x must be of the same class (RasterStack, RasterBrick or SpatialPointsDataFrame)")
  }

  if (class(x[[1]]) %in% "SpatialPointsDataFrame"){
    calcMode <- "sp"
  }else{
    calcMode <- "raster"
  }

  if(length(x) <= 1) {
    stop("x must have at least 2 elements to calculte temporal metrics from")
  }

  if (length(filename) > 1) {
    stop("Provide only one filename. Names of the processed spectral indices are automatically appended.")
  }

  # Define variables as NULL (fix "no visible binding for global variable" note during check)
  y <- NULL


  # Check if layer names are equals and display warning + set generic indices names if they are not
  list_indices <- lapply(x, names)

  #Check that each object has the same number of layers/columns
  list_nlayers <- lapply(list_indices, length)
  if(!isTRUE(do.call(all.equal, list_nlayers))) {
    stop("Elements of x do not have the same number of indices")
  }else{
    nindices <- list_nlayers[[1]]
  }

  # Check that names ar not duplicated
  if (any(unlist(lapply(list_indices, anyDuplicated)) > 0)) {
    stop("Elements of x cannot have duplicated layer names")
  }

  # Same all layers have the same indices name, even if not in same order
  # If not, set generic VI names
  ref_names <- list_indices[[1]] # Use names of first element as ref

  if(all(unlist(lapply(list_indices, function(x) {
    all(x %in% ref_names)
  })))) {
    indices_names <- ref_names
  }else{
    warning("Layer names do not match. Setting generic indices names.")
    indices_names <- paste0("VI",seq(1:nindices))
    x <- lapply(x, function(x) {
      names(x) <- indices_names
      return(x)
    })
  }

  # Check temporal summary function

  if (!is.character(metrics)) stop("metrics must be a character (function name)")
  if (any(grepl("\\(|\\)", metrics))) stop("metrics must be a character without
                                         brackets ()")

  eval_fun <- character()
  for (f in 1:length(metrics)) {
    eval_fun[f] <- paste0(metrics[f], "(value)")
  }

  eval_fun <- paste(eval_fun, collapse = ",")

  # Check that list elements have the same structure

  if(calcMode == "raster") {
    if(!do.call(raster::compareRaster,
            c(x, extent = TRUE, rowcol = TRUE,crs = TRUE, res = TRUE, orig = TRUE, rotation = FALSE))) {
      stop("Rasters of the list x do not have the same properties (either extent, dimension, crs, resolution or origin")
    }
  }else if (calcMode == "sp") {
    list_nrows <- lapply(x, nrow)
    list_crs <- lapply(x, raster::crs)

    if(!isTRUE(do.call(all.equal, list_nrows))) {
      stop("SpatialPointsDataFrame of x don't have the same number of features")
    }

    if(!isTRUE(do.call(all.equal, list_crs))) {
      stop("SpatialPointsDataFrame of x don't have the same CRS")
    }

    x <- lapply(x, function(x) {
      sp::coordnames(x) <- c("x", "y")
      return(x)
    })

    # Check that coordinates are the same
    if(!isTRUE(do.call(all.equal, lapply(x, sp::coordinates)))) {
      stop("SpatialPointsDataFrame coordinates of x not matching")
    }
  }

  ## Big for loop for each VI ##

  # Check if printing progress bar
  if(progress) progress = "text" else progress = NULL

  #Initiate list that will be returned
  out <- list()

  for(index in indices_names) {
    message(sprintf("Working on %s",index))

    if (calcMode == "sp") {

      # Prepare spdf for processing
      toProcess <- lapply(x, function(x) {
        out <- x[,index]
        return(out)
      })

      toProcess <- do.call(cbind, toProcess)
      ind_df <- spdf2df(toProcess, xy = TRUE)

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
        data = result[, -c("x", "y")], proj4string = list_crs[[1]]
      )
      names(result) <- paste0(index, "_", names(result))

      if (filename != "") {
        layer_name <- paste(tools::file_path_sans_ext(basename(filename)),index, sep = "_")
        rgdal::writeOGR(result,
                        driver = "ESRI Shapefile",
                        layer = layer_name,
                        dsn = dirname(filename), ...
        )
      }

    }else if (calcMode == "raster"){

      #Prepare RasterStack for processing
      toProcess <- lapply(x, function(x) {
        raster::subset(x, index)
      })

      toProcess <- raster::stack(toProcess)

      if (filename != "") {
        fname <- file.path(dirname(filename), paste0(tools::file_path_sans_ext(basename(filename)),"_",index,".",tools::file_ext(filename)))
      }else{
        fname <- ""
      }

      if (par) {
        raster::beginCluster(threads)
        result <- raster::clusterR(toProcess,
                                   fun = raster::calc,
                                   args = list(fun = eval(parse(text = metrics))),
                                   filename = fname, m = m, progress = progress,
                                   ...
        )
        raster::endCluster()
      } else {
        result <- raster::calc(toProcess,
                               fun = eval(parse(text = metrics)),
                               filename = fname, progress = progress,
                               ...
        )
        names(result) <- paste0(index, "_", names(result))
      }
    }

    out[[index]] <- result
  }

  if (stack == TRUE) {
    if(calcMode == "raster") {
      out <- do.call(raster::stack, unname(out))
    }else if (calcMode == "sp") {
      out <- do.call(cbind, out)
    }
  }

  return(out)
}
