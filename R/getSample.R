#'Stratified random sampling
#'
#'Performs kmeans clustering to stratify \code{x} and randomnly sample within the strata until \code{n} samples are selected. The number of samples selected in each starta is proportional to the occurence of those strata in \code{x}.
#'
#'\code{x} is stratified using kmean clustering from \code{\link[RStoolbox]{unsuperClass}}. By default, clustering is performed on a random subset of \code{x} and run with multiple starting condigurations in order to find a convergent solution from the multiple starts. The parameters controling the number of random samples used to perform kmeans clustering and the number of starting configurations can be provided under the \code{...} argument. More information on the behaviour of the kmeans clustering can be found in \code{\link[RSToolbox]{unsuperClass}}. The default kmean clustering method is Hartigan-Wong algorithm. The algorithm might not converge and output "Quick Tansfer" warning. If this is the case, we suggest decreasing \code{strata}. Also, if \code{minDist} is too large, it might not be possbile to select enough samples per strata. In that case, the warning "Exeeded maximum number of runs for strata" is displayed.
#'
#'@param x A \code{Raster*} object used to generate random sample
#'@param strata Number of strata. Default is 5.
#'@param layers Vector indicating the bands of \code{x} used in stratification (as integer or names). By default, all layers of x are used.
#'@param norm Logical. If TRUE (default), \code{x} is normalized before k-means clustering. This is useful if \code{layers} have different scales.
#'@param n Sample size
#'@param mindist Minimum distance between samples (in units of \code{x}). Default is 0.
#'@param maxIter Numeric. This number is multiplied to the number of samples to select per strata. If the number of iterations to select samples exceeds maxIter x the number of samples to select then the loop will break and a warnign message returned. Default is 30.
#'@param xy Logical indicating if X and Y coordinates of samples should be included in the fields of the returned \code{\link[sp]{SpatialPointsDataFrame}} object. Only relevant if \code{sp= TRUE}
#'@param filename_cluster Character. Filename of the clustered \code{x} raster if saved to disk.
#'@param filename_samples Character. Filename of the samples saved to disk as an ESRI Shapefile. It is not necessary to add extension.
#'@param ... Further arguments passed to \code{\link[RStoolbox]{unsuperClass}} or \code{\link[raster]{writeRaster}} to control the kmeans algorithm or writing parameters
#'@return A list with a \code{SpatialPointsDataFrame} object containing sample locations and the output of \code{\link[RSToolbox]{unsuperClass}}
#'@export
#'@importFrom dplyr %>%

getSample <- function(x,
                      strata=5,
                      layers = names(x),
                      norm=TRUE,
                      n,
                      mindist = 0,
                      maxIter=30,
                      xy=T,
                      filename_cluster='',
                      filename_samples='',
                      ...) {

  if(!class(x)[1] %in% c('RasterLayer','RasterStack','RasterBrick')){
    stop("x must be a Raster object")
  }

  #Select layers of x used to compute kmean
  x.layers <- raster::subset(x,layers,drop=TRUE)

  x.clustered <- RStoolbox::unsuperClass(img = x.layers, nClasses = strata, norm = norm, filename=filename_cluster, ...)

  rr <- data.frame(raster::rasterToPoints(x.clustered$map,spatial=FALSE))
  colnames(rr) <- c("x","y","cluster")

  #determine number of samples for each strata
  samples_count <- rr %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(count=dplyr::n()) %>%
    dplyr::mutate(p=count/sum(count)) %>%
    dplyr::mutate(n_samples = floor(p * n)) %>%
    dplyr::arrange(desc(n_samples))

  #total number of sample may be less than n, because floor() is used
  #the difference is added randomly
  add_samples_count <- n-sum(samples_count$n_samples)
  for (i in 1:add_samples_count) {
    row_id <- sample(nrow(samples_count), 1)
    samples_count[row_id, ]$n_samples <- samples_count[row_id, ]$n_samples + 1
  }

  # Select first random sample in first strata
  rr_temp <- dplyr::filter(rr, cluster == samples_count$cluster[1])
  samples <- rr_temp[sample(nrow(rr_temp), 1), ]

  #the rest of samples is selected one by one
  #for every new sample a distance is calculated to all existing samples
  #the new candidate sample is added if the distance is less than mindist
  for (strata in samples_count$cluster) {
    count_runs <- 0
    print(sprintf("cluster %d: %d samples to select",strata,as.integer(samples_count[samples_count$cluster==strata,"n_samples"])))

    #check if enough samples in strata
    while(nrow(dplyr::filter(samples, cluster==strata)) <   dplyr::filter(samples_count, cluster==strata)$n_samples) {

      current_rr <- dplyr::filter(rr, cluster == strata)

      #select another random sample
      candidate <- current_rr[sample(nrow(current_rr), 1), ]

      #check if distance to existing samples is less than mindist
      distances <- spatstat::crossdist(samples$x, samples$y, candidate$x, candidate$y)
      if(!any(as.numeric(distances) < mindist)) {
        samples <- rbind(samples, candidate)
        current_rr <- current_rr[setdiff(rownames(current_rr),rownames(candidate)),]
      }

      if(nrow(current_rr)==0){
        warning("All possible candidate cells for strata %d have been considered and the number of samples to select couldn't be reached")
        break
      }

      #protect agaist endless loops
      count_runs<- count_runs+1
      if (count_runs >= maxIter * dplyr::filter(samples_count, cluster==strata)$n_samples) {
        warning("Exeeded maximum number of runs for strata ",strata)
        break
      }
    }
  }


  if(xy){
      samples <- SpatialPointsDataFrame(coords = dplyr::select(samples, x, y), proj4string=crs(x),data=samples)
  }else{
      samples <- SpatialPointsDataFrame(coords = dplyr::select(samples, x, y), proj4string=crs(x),data=dplyr::select(samples,-c("x","y")))
  }

  toReturn <- list(samples=samples,
                  cluster=x.clustered)

  if (filename_samples != ''){
    rgdal::writeOGR(samples,driver="ESRI Shapefile",layer=tools::file_path_sans_ext(basename(filename_samples)),dsn=dirname(filename_samples),...)
  }

  return(toReturn)
}
