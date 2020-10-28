#' Stratified random sampling
#'
#' Performs kmeans clustering to stratify \code{x} and randomly samples within
#' the strata until \code{n} samples are selected. The number of samples selected
#' in each strata is proportional to the occurrence of those strata across the
#' classified raster.
#'
#' \code{x} is stratified using kmeans clustering from \code{\link[RStoolbox]{unsuperClass}}.
#' By default, clustering is performed on a random subset of \code{x} (10000 cells) and run
#' with multiple starting configurations in order to find a convergent solution
#' from the multiple starts. The parameters controlling the number of random
#' samples used to perform kmeans clustering and the number of starting
#' configurations can be provided as additional \code{...} arguments. More
#' information on the behavior of the kmeans clustering can be found in
#' \code{\link[RStoolbox]{unsuperClass}}. The default kmeans clustering method
#' is Hartigan-Wong algorithm. The algorithm might not converge and output
#' "Quick Transfer" warning. If this is the case, we suggest decreasing
#' \code{strata}. Also, if \code{mindist} is too large, it might not be
#' possible to select enough samples per strata. In that case, the warning
#' "Exceeded maximum number of runs for strata" is displayed. In that case
#' you can decrease the number of samples \code{n} or increase \code{maxIter}
#' to control the number of maximum iterations allowed until the required number of samples are selected.
#'
#' @param x A \code{Raster*} object used to generate random sample
#' @param strata Number of strata (kmeans clusters). Default is 5.
#' @param layers Vector indicating the bands of \code{x} used in stratification
#' (as integer or names). By default, all layers of x are used.
#' @param norm Logical. If TRUE (default), \code{x} is normalized before k-means
#' clustering. This is useful if \code{layers} have different scales.
#' @param n Sample size
#' @param mindist Minimum distance between samples (in units of \code{x}). Default is 0.
#' @param maxIter Numeric. This number is multiplied to the number of samples to select per strata. If the number of iterations to select samples exceeds maxIter x the number of samples to select then the loop will break and a warning message be returned. Default is 30.
#' @param xy Logical indicating if X and Y coordinates of samples should be included in the fields of the returned \code{\link[sp]{SpatialPoints}} object.
#' @param filename_cluster Character. Output filename of the clustered \code{x} raster including path to directory and eventually extension
#' @param filename_sample Character. Output filename of the sample points including path to directory. File will be automatically saved as an ESRI Shapefile and any extension in \code{filename_sample} will be overwritten
#' @param ... Further arguments passed to \code{\link[RStoolbox]{unsuperClass}}, \code{\link[raster]{writeRaster}} or \code{\link[rgdal]{writeOGR}} to control the kmeans algorithm or writing parameters
#' @return A list with the following objects:
#'    \describe{
#'        \item{\code{sample}}{A \code{\link[sp]{SpatialPoints}} object containing sampled points}
#'        \item{\code{clusterMap}}{The clustered \code{x} raster, output of \code{\link[RStoolbox]{unsuperClass}}}
#'        \item{\code{model}}{The kmeans model, output of \code{\link[RStoolbox]{unsuperClass}}}
#'    }
#'
#' @seealso \code{\link[RStoolbox]{unsuperClass}}
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
#' # Sample 5 cells in 3 strata (kmeans clusters). Sampled points should be at least 30 m apart.
#' set.seed(1234) #for example reproducibility
#' sample_strata <- getSample(Y_vars,
#'                            n = 5,
#'                            strata = 3,
#'                            mindist = 30)
#' @export
#' @importFrom dplyr %>%

getSample <- function(x,
                      strata = 5,
                      layers,
                      norm = TRUE,
                      n,
                      mindist = 0,
                      maxIter = 30,
                      xy = TRUE,
                      filename_cluster = "",
                      filename_sample = "",
                      ...) {
  if (!class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    stop("x must be a Raster object")
  }

  # Define variables as NULL (fix "no visible binding for global variable" note during check)
  cluster <- p <- count <- n_samples <- y <- NULL

  # Select layers of x used to compute kmean
  if(missing(layers)) layers = names(x)

  x.layers <- raster::subset(x, layers, drop = TRUE)

  #Before sending to RSToolboox::unsuperClass, fix filename for cluster map
  # wrArgs <- list(...)
  # clusterMap <- ifelse("clusterMap" %in% names(wrArgs), clusterMap, TRUE)
  # atMax <- ifelse("clusterMap" %in% names(wrArgs), clusterMap, 10000)
  # FULL <- !clusterMap | atMax && raster::canProcessInMemory(x.layers, n = 4)

  if (gsub(" ", "", filename_cluster) == "") {
    x.clustered <- RStoolbox::unsuperClass(img = x.layers, nClasses = strata,
                                           norm = norm, ...)
  }else{
    x.clustered <- RStoolbox::unsuperClass(img = x.layers, nClasses = strata,
                                           norm = norm, filename = filename_cluster, ...)
  }




  rr <- data.frame(raster::rasterToPoints(x.clustered$map, spatial = FALSE))
  colnames(rr) <- c("x", "y", "cluster")

  # determine number of samples for each strata
  samples_count <- rr %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(p = count / sum(count)) %>%
    dplyr::mutate(n_samples = floor(p * n)) %>%
    dplyr::arrange(dplyr::desc(n_samples))

  # total number of sample may be less than n, because floor() is used
  # the difference is added randomly
  add_samples_count <- n - sum(samples_count$n_samples)
  for (i in 1:add_samples_count) {
    row_id <- sample(nrow(samples_count), 1)
    samples_count[row_id, ]$n_samples <- samples_count[row_id, ]$n_samples + 1
  }

  # Select first random sample in first strata
  rr_temp <- dplyr::filter(rr, cluster == samples_count$cluster[1])
  samples <- rr_temp[sample(nrow(rr_temp), 1), ]

  # the rest of samples is selected one by one
  # for every new sample a distance is calculated to all existing samples
  # the new candidate sample is added if the distance is less than mindist
  for (strata in samples_count$cluster) {
    count_runs <- 0
    message(sprintf("cluster %d: %d samples to select", strata,
      as.integer(samples_count[samples_count$cluster == strata, "n_samples"])))

    # check if enough samples in strata
    while (nrow(dplyr::filter(samples, cluster == strata)) <
           dplyr::filter(samples_count, cluster == strata)$n_samples) {
      current_rr <- dplyr::filter(rr, cluster == strata)

      # select another random sample
      candidate <- current_rr[sample(nrow(current_rr), 1), ]

      # check if distance to existing samples is less than mindist
      distances <- spatstat::crossdist(samples$x, samples$y, candidate$x,
                                       candidate$y)
      if (!any(as.numeric(distances) < mindist)) {
        samples <- rbind(samples, candidate)
        current_rr <- current_rr[setdiff(rownames(current_rr),
                                         rownames(candidate)), ]
      }

      if (nrow(current_rr) == 0) {
        warning("All possible candidate cells for strata %d have been considered
                and the number of samples to select couldn't be reached")
        break
      }

      # protect agaist endless loops
      count_runs <- count_runs + 1
      if (count_runs >= maxIter * dplyr::filter(samples_count,
                                                cluster == strata)$n_samples) {
        warning("Exceeded maximum number of runs for strata ", strata)
        break
      }
    }
  }

  if (xy) {
    samples <- sp::SpatialPointsDataFrame(coords = dplyr::select(samples, x, y),
                                      proj4string = raster::crs(x), data = samples)
  } else {
    samples <- sp::SpatialPointsDataFrame(coords = dplyr::select(samples, x, y),
              proj4string = raster::crs(x), data = dplyr::select(samples, -c("x", "y")))
  }

  toReturn <- list(
    sample = samples,
    clusterMap = x.clustered$map,
    model = x.clustered$model
  )

  if (filename_sample != "") {
    rgdal::writeOGR(samples, driver = "ESRI Shapefile",
      layer = tools::file_path_sans_ext(basename(filename_sample)),
      dsn = dirname(filename_sample), ...)
  }

  return(toReturn)
}
