require(dplyr)
require(spatstat)
#'Generate a minimum distance stratified random sample
#'
#'@param x A raster layer used to generate random sample
#'@param n Sample size
#'@param mindist Minimum distance between samples (in meters)
#'@return A \code{SpatialPointsDataFrame} object containing sample locations

getSampleXY <- function(x, n, mindist = 500) {
  # stratify using knn
  r_clustered <- RStoolbox::unsuperClass(img = x, nClasses = 10, norm = T)

  #convert to df
  rr <- raster::as.data.frame(r_clustered$map, xy=T)

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

  #the rest of samples is selected one by one
  #for every new sample a distance is calculated to all existing samples
  #the new candidate sample is added if the distance is less than mindist
  for (strata in samples_count$layer) {
    count_runs <- 0

    #check if enough samples in strata
    while(nrow(dplyr::filter(samples, layer==strata)) <   dplyr::filter(samples_count, layer==strata)$n_samples) {

      current_rr <- dplyr::filter(rr, layer == strata)

      #select another random sample
      candidate <- current_rr[sample(nrow(current_rr), 1), ]

      #check if distance to existing samples is less than mindist
      distances <- spatstat::crossdist(samples$x, samples$y, candidate$x, candidate$y)
      if(!any(as.numeric(distances) < mindist)) {
        samples <- rbind(samples, candidate)
      }

      #protect agaist endless loops
      count_runs<- count_runs+1
      if (count_runs >= 10 * dplyr::filter(samples_count, layer==strata)$n_samples) {
        print(paste0("Exeeded maximum number of runs for strata ",strata))
        break
      }
    }
  }
  samples <- SpatialPointsDataFrame(coords = dplyr::select(samples, x, y), data=samples)
  return(samples)
}




#'Get pixel values for sample locations
#'
#'@param x A raster layer
#'@param s Locations of the samples. Object of class \code{SpatialPointsDataFrame}. Can be generated with \code{getSampleXY}
#'@return A \code{SpatialPointsDataFrame} object containing pixel values corresponding to sample locations

GetSampleValues <- function(x, s) {


    values <- as.data.frame(x[cellFromXY(x, s)])
    values <- SpatialPointsDataFrame(coords = coordinates(s), data=values)
    return(values)

}



# s <- getSamplesXY(r, 100)
# dim(s)
# class(s)
# plot(s)
# s
# r3 <- multiFocal(r, w=matrix(1/9,nrow=3,ncol=3))
# r_agr <- aggregate(r, fact=3)
#
# tttt <- GetSampleValues(r, s)
# tttt <- GetSampleValues(r_agr, s)




# GetSampleValues <- function(x, s, expand=F) {
#   #get values for samples
#
#   #convert to df
#   r.df <- as.data.frame(x, xy=T)
#
#   #add key
#   r.df$key <- paste0(r.df$x,"_",r.df$y)
#
#   #add key
#   s$key <- paste0(s$x,"_",s$y)
#
#   if(expand == F) {
#
#     values <- filter(r.df, key %in% s$key) %>% select(-key)
#   }
#   if(expand == T) {
#     #need to expand samples and average values
#     #get resolution
#     pix <- res(x)[1]
#
#     for (i in 1:nrow(s)) {
#       a <- s[i,]
#       mod_x <- c(-1,0,1,-1,0,1,-1,0,1)
#       mod_y <- c(1,1,1,0,0,0,-1,-1,-1)
#
#       if (i == 1) {
#         keys <- data.frame(key=paste0(a$x + pix*mod_x,"_",a$y + pix*mod_y),id=a$key)
#       } else {
#         keys <- rbind(keys,data.frame(key=paste0(a$x + pix*mod_x,"_",a$y + pix*mod_y),id=a$key))
#       }
#     }
#     values <- merge(r.df, keys, by="key")
#     values %<>%
#       select(-key) %>%
#       group_by(id) %>%
#       summarise_all(mean, na.rm = TRUE) %>%
#       select(-id)
#
#   }
#
#   values <- SpatialPointsDataFrame(coords = select(values, x, y), data=values)
#   return(values)
# }

# tttt <- GetSampleValues(r, s)

# dim(tttt@data)
# s <- as.data.frame(s)
# s %<>% select(x,y)

# values <- list()


















# GetSampleValues(r, s)

# GetSampleValues <- function(x, s) {
#
#   if(is.na(window)) {
#     values <- as.data.frame(r[cellFromXY(r, s)])
#   }
#   if(window == "3x3") {
#     #expand cells
#
#     #get cells
#     cells <- cellFromXY(r, s)
#     #get rows and cols
#     RowsCols <- rowColFromCell(object = r, cell = cells)
#
#   }
#
# }

# values <- list()
# for (i in 1:nrow(RowsCols)) {
#   print(i)
#
#
#   a <- RowsCols[i,] #current RowCol
#   #get 3x3 window
#   rows3x3 <- c(a[1]-1,a[1],a[1]+1,a[1]-1,a[1],a[1]+1,a[1]-1,a[1],a[1]+1)
#   cols3x3 <- c(a[2]+1,a[2]+1,a[2]+1, a[2],a[2],a[2],a[2]-1,a[2]-1,a[2]-1)
#
#   values3x3 <- r[cellFromRowCol(r, rows3x3, cols3x3)]
#
#   #average values
#   values[[i]] <- colMeans(values3x3,na.rm = T)
#
# }

# c(a[1]-1, a[2]+1)
# c(a[1], a[2]+1)
# c(a[1]+1, a[2]+1)
# c(a[1]-1, a[2])
# c(a[1], a[2])
# c(a[1]+1, a[2])
# c(a[1]-1, a[2]-1)
# c(a[1], a[2]-1)
# c(a[1]+1, a[2]-1)







#
# cellFromXY(r, s[,c(1,2)])
# r[cellFromXY(r, s[,c(1,2)])]
#
#
# r[cellFromXY(r, c(417930,5421750))]
#
#
#
#
# 417930 5421750                    8.018213                   19.987158                   74.828316
#
#




# function to generate samples
# getSamples <- function(x, n, method="random") {
#   #check if method is correct
#   stopifnot(method %in% c("random", "stratified_random"))
#
#   #x can be a raster or rasterstack
#
#   if (method=="random") {
#     s <- raster::sampleRandom(x = x, size = n, sp=T, xy=T)
#   }
#   if (method=="stratified_random") {
#     # run knn clustering to get strata
#     r_clustered <- RStoolbox::unsuperClass(img = x, nClasses = 10, norm = T)
#
#     #calculate pr
#     s <- raster::sampleStratified(x = test$map, n, sp=T, xy=T)
#   }
#
#   return(s)
#
# }
# tt <- getSamples(x=r, 1000, method = "stratified_random")
#
# mid(tt@data)
# tt@data %>% group_by(layer) %>% summarise(n())





# #
# # r_clustered <- RStoolbox::unsuperClass(img = r, nClasses = 10, norm = T)
# # rr <- as.data.frame(r_clustered$map, xy=T)
# #
# #
# # n <- 100
# # #determine number of samples for each strata
# # samples_count <- rr %>%
# #   dplyr::filter(!is.na(layer)) %>%
# #   group_by(layer) %>%
# #   summarise(count=n()) %>%
# #   mutate(p=count/sum(count)) %>%
# #   mutate(n_samples = floor(p * n)) %>%
# #   arrange(n_samples)
# #
# # #sample strata
# # mid(rr)
# #
#
#
# mindist <- 500
#
# #first random sample
# rr_temp <- filter(rr, layer == samples_count$layer[1])
# samples <- rr_temp[sample(nrow(rr_temp), 1), ]
#
# for (strata in samples_count$layer) {
#   print(strata)
#
#   #check if enough samples in strata
#   count_runs <- 0
#   while(nrow(filter(samples, layer==strata)) <   filter(samples_count, layer==strata)$n_samples) {
#
#     current_rr <- filter(rr, layer == strata)
#
#     #select another random sample
#     candidate <- current_rr[sample(nrow(current_rr), 1), ]
#
#     #check if distance to existing samples is less than mindist
#     distances <- crossdist(samples$x, samples$y, candidate$x, candidate$y)
#     if(!any(as.numeric(distances) < mindist)) {
#       samples <- rbind(samples, candidate)
#     }
#
#     #protect agaist endless loops
#     count_runs<- count_runs+1
#     if (count_runs >= 2*filter(samples_count, layer==strata)$n_samples) {
#       print(paste0("Exeeded maximum number of runs for strata ",strata))
#       break
#     }
#   }
# }
#
#
#
#
#
#
# samples %>% group_by(layer) %>% summarise(n())
#
#
#
#
#
# #select random samples with a distance constraint and proportionally to strata size
#
# rr2 <- filter(rr,!is.na(layer))
#
#
#
#
# #first random sample
#
# samples <- rr2[sample(nrow(rr2), 1), ]
#
# count_runs <- 0
# while(length(samples$x) < n) {
#
#   #select another random sample
#   candidate <- rr2[sample(nrow(rr2), 1), ]
#
#   #check if distance to existing samples is less than mindist
#   distances <- crossdist(samples$x, samples$y, candidate$x, candidate$y)
#   if(!any(as.numeric(distances) < mindist)) {
#     samples <- rbind(samples, candidate)
#   }
#
#   count_runs<- count_runs+1
#   if (count_runs >= 2*n) stop("Exeeded maximum number of runs")
# }
#
