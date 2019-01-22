#'Stratified random sampling
#'
#'Generate a minimum distance stratified random sample
#'
#'\code{x} is stratified using kmean clustering from \code{\link[RStoolbox]{unsuperClass}}. Default kmean clustering method is Hartigan-Wong algorithm. The algorithm might not converge and output "Quick Tansfer" warning. If this is the case, we suggest decreasing \code{nClasses}. Also, if \code{minDist} is too large, the algorithm might not be able to select enough sample per strata. In that case, the warning "Exeeded maximum number of runs for strata" is displayed.
#'
#'@param x A \code{Raster*} object used to generate random sample
#'@param layers Vector containing the band numbers of \code{x} used in stratification. By default, all layers of x are used
#'@param n Sample size
#'@param mindist Minimum distance between samples (in units of \code{x}). Default is 0.
#'@param nClasses Number of strata. Default is 5.
#'@param ... Further arguments passed to \code{\link[RStoolbox]{unsuperClass}} or \code{\link[raster]{writeRaster}} to control the kmeans algorithm or writing parameters
#'@return A \code{SpatialPointsDataFrame} object containing sample locations

getSampleXY <- function(x,
                        layers = seq(1,raster::nlayers(x),1),
                        n,
                        mindist = 0,
                        nClasses = 5,
                        ...) {

  if(!class(x)[1] %in% c('RasterLayer','RasterStack','RasterBrick')){
    stop("x must be a Raster object")
  }

  #Select layers of x used to compute kmean
  x.layers <- x[[layers]]

  x.clustered <- RStoolbox::unsuperClass(img = x.layers, nClasses = nClasses, norm = T, ...)

  rr <- raster::as.data.frame(x.clustered$map,xy=T)
  rr$cellID <- rownames(rr)
  #Parallel kmean using knor::Kmeans. Only one starting config?
  #Preparing kmean input
  #km_dat <- raster::as.data.frame(x_kmean,xy=T,na.rm=T)
  #km_dat <- as.matrix(km_dat)

  #print(sprintf("Calculating %d clusters on %d cores",nClasses,no_cores))
  #km <- knor::Kmeans(km_dat[,!colnames(km_dat) %in% c("x","y")],centers = nClasses,nthread = no_cores)
  #print("Done")

  #rr <- data.frame(layer=km$cluster,km_dat[,c("x","y")])

  # kmean using RStoolbox
  # # stratify using knn
  # r_clustered <- RStoolbox::unsuperClass(img = x, nClasses = nClasses, norm = T)
  #
  # #convert to df
  # rr <- raster::as.data.frame(r_clustered$map, xy=T)

  #determine number of samples for each strata
  samples_count <- rr %>%
    dplyr::filter(!is.na(layer)) %>%
    dplyr::group_by(layer) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::mutate(p=count/sum(count)) %>%
    dplyr::mutate(n_samples = floor(p * n)) %>%
    dplyr::arrange(n_samples)

  #total number of sample may be less than n, because floor() is used
  #the difference is added randomly
  add_samples_count <- n-sum(samples_count$n_samples)
  for (i in 1:add_samples_count) {
    row_id <- sample(nrow(samples_count), 1)
    samples_count[row_id, ]$n_samples <- samples_count[row_id, ]$n_samples + 1
  }

  #first random sample
  rr_temp <- dplyr::filter(rr, layer == samples_count$layer[1])
  samples <- rr_temp[sample(nrow(rr_temp), 1), ]

  print(sprintf("Selecting %d stratified samples based on mindist %d",n,mindist))

  #the rest of samples is selected one by one
  #for every new sample a distance is calculated to all existing samples
  #the new candidate sample is added if the distance is less than mindist
  for (strata in samples_count$layer) {
    count_runs <- 0
    print(sprintf("Layer %d (%d samples to select)",strata,as.integer(samples_count[samples_count$layer==strata,"n_samples"])))

    #check if enough samples in strata
    while(nrow(dplyr::filter(samples, layer==strata)) <   dplyr::filter(samples_count, layer==strata)$n_samples) {

      current_rr <- dplyr::filter(rr, layer == strata)

      #select another random sample
      candidate <- current_rr[sample(nrow(current_rr), 1), ]

      #check if distance to existing samples is less than mindist
      distances <- spatstat::crossdist(samples$x, samples$y, candidate$x, candidate$y)
      if(!any(as.numeric(distances) < mindist)) {
        samples <- rbind(samples, candidate)
        current_rr <- current_rr[setdiff(rownames(current_rr),rownames(candidate)),]
      }

      #protect agaist endless loops
      count_runs<- count_runs+1
      if (count_runs >= 20 * dplyr::filter(samples_count, layer==strata)$n_samples) {
        warning("Exeeded maximum number of runs for strata ",strata)
        break
      }
    }
  }


  samples <- SpatialPointsDataFrame(coords = dplyr::select(samples, x, y), proj4string=crs(x),data=select(samples,-c("x","y")))

  return(samples)
}
