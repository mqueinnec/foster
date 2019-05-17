## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=12, 
  fig.height=8,
  out.width="100%")
library(foster)
library(ggplot2)
library(raster)
library(knitr)
set.seed(1234)
  

## ---- echo=FALSE, out.width='100%'---------------------------------------
knitr::include_graphics(system.file("extdata/workflow/diagram_foster_compact_v2.png",package="foster"))

## ------------------------------------------------------------------------
library(foster)
library(raster)

## ------------------------------------------------------------------------
#Read single layer raster
elev_p95 <- raster(system.file("extdata/inputs/lidar/p95_lines_small.tif",package="foster"))
#Read muli-layer raster
spectral_1984 <- stack(system.file("extdata/inputs/spectral/Mosaic_SRef_UTM10S_1984_proxy_v2.tif",package="foster"))

## ------------------------------------------------------------------------
dem_samples <- rgdal::readOGR(dsn = system.file("extdata/inputs/ref_table/",package = "foster"), layer = "dem_sample",verbose=F)

## ---- eval=F-------------------------------------------------------------
#  #Example to write output of calcIndices to disk using different options
#  ind <- calcIndices(x, indices = c("NDVI","TCG","TCW"),red = 3,nir=4,filename = "full/path/to/filename.tif")
#  ind <- calcIndices(x, indices = c("NDVI","TCG","TCW"),red = 3,nir=4,filename = "full/path/to/filename", format="GTiff", overwrite=T, byLayer=T)

## ---- eval=F-------------------------------------------------------------
#  rasterOptions(tmpdir) <- "path/to/tempdir"

## ------------------------------------------------------------------------
elev_p95 <- raster(system.file("extdata/inputs/lidar/p95_lines_small.tif",package="foster"))
elev_std <- raster(system.file("extdata/inputs/lidar/std_lines_small.tif",package="foster"))
Y_vars <- stack(elev_p95,elev_std)
#Set up layers names
names(Y_vars) <- c("p95","std")
Y_vars
plot(Y_vars)

## ------------------------------------------------------------------------
spectral_1984 <- stack(system.file("extdata/inputs/spectral/Mosaic_SRef_UTM10S_1984_proxy_v2.tif",package="foster"))
names(spectral_1984) <- c("blue","green","red","nir","swir1","swir2")
spectral_1984
plot(spectral_1984)

## ------------------------------------------------------------------------
mask_forest <- raster(system.file("extdata/inputs/landcover/forested.tif",package="foster"))
plot(mask_forest)

## ------------------------------------------------------------------------
Y_vars_resampled <- matchResolution(Y_vars,spectral_1984,method='bilinear',filename='')
Y_vars_resampled

## ------------------------------------------------------------------------
#We use mask=F because we don't want to mask Y_vars_resampled with spectral_1984
Y_vars_extend <- matchExtent(Y_vars_resampled,spectral_1984,mask=F)
Y_vars_extend

## ------------------------------------------------------------------------
filt <- matrix(1,nrow=3,ncol=3)
Y_vars_smooth <- focalMultiBand(Y_vars_extend,w=filt,fun=mean,pad=T,padValue=NA, na.rm=T, keepNA = T, filename='')
plot(Y_vars_smooth[[1]])

## ---- out.width="100%"---------------------------------------------------
Y_vars_mask <- matchExtent(Y_vars_smooth,mask_forest,mask=T,maskValue = NA,filename='')
plot(Y_vars_mask[[1]])

## ------------------------------------------------------------------------
spectral_trgs <- matchExtent(spectral_1984,Y_vars_smooth, mask=T, inverse = T, filename= '')
spectral_trgs <- matchExtent(spectral_trgs, mask_forest, mask = T, maskValue = NA, filename = "")
plot(spectral_trgs[["nir"]])

## ------------------------------------------------------------------------
Y_vars_edges <- edges(Y_vars_mask,w=3,filename='')
plot(Y_vars_edges[[1]])

## ------------------------------------------------------------------------
nSamples = 230
nClasses = 5
mindist = 75

set.seed(1234) #For example reproducibility
samples <- getSample(Y_vars_edges, layers = c("p95","std"), n = nSamples, strata = nClasses, mindist = mindist, norm = T,xy = T,sp=T)
samples_points <- samples$samples
samples_points

## ------------------------------------------------------------------------
plot(samples$cluster$map)
plot(samples_points,add=T)

## ------------------------------------------------------------------------
ind_list <- c("TCB","TCW","TCG",'NDVI')
ind <- calcIndices(spectral_trgs,indices = ind_list, sat="Landsat5TM", red=3, nir=4)
plot(ind[["TCG"]])

## ------------------------------------------------------------------------
ind_smooth <- focalMultiBand(ind,w=filt,fun=mean,na.rm=T,pad = T,keepNA = T)
plot(ind_smooth[["TCG"]])

## ------------------------------------------------------------------------
funSummary <- function(x){
  
  c(
    median = median(x,na.rm=T),
    IQR = IQR(x,na.rm=T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}

## ------------------------------------------------------------------------
NDVI_series <- stack(system.file("extdata/inputs/indices_stack/all_NDVI_stack.tif",package="foster"))
names(NDVI_series) <- paste0("NDVI_",as.character(seq(1984,2008,1)))
NDVI_series

ind_metrics <- temporalMetrics(NDVI_series,metrics='funSummary', prefix = "NDVI", filename = '')
ind_metrics
plot(ind_metrics[["NDVI_median"]])

## ------------------------------------------------------------------------
#Extract values of ind_metrics at samples
ind_metrics_sample <- getSampleValues(ind_metrics,samples_points)

#Extract values of NDVI_series at samples and calculate temporal metrics at samples only
NDVI_series_samples <- getSampleValues(NDVI_series, samples_points)
NDVI_metrics_sample <- temporalMetrics(NDVI_series_samples,metrics="funSummary",filename='') 

#Compare the two objects 
head(ind_metrics_sample)
head(NDVI_metrics_sample)

## ------------------------------------------------------------------------
Y_vars_samples <- rgdal::readOGR(system.file("extdata/inputs/ref_table/Y_vars_sample.shp",package = "foster"),verbose=F)
ind_metrics_samples <- rgdal::readOGR(system.file("extdata/inputs/ref_table/ind_metrics_sample.shp",package = "foster"),verbose=F)
dem_samples <- rgdal::readOGR(system.file("extdata/inputs/ref_table/dem_sample.shp",package = "foster"),verbose=F)
slope_samples <- rgdal::readOGR(system.file("extdata/inputs/ref_table/slope_sample.shp",package = "foster"),verbose=F)

samples_all <- cbind(Y_vars_samples,ind_metrics_samples,dem_samples,slope_samples)
names(samples_all) <- c("p95","std","TCG_median","TCG_IQR","TCG_slope","TCW_median","TCW_IQR","TCW_slope","TCB_median","TCB_IQR","TCB_slope","NDVI_median","NDVI_IQR","NDVI_slope","DEM", "DEM_slope")

## ------------------------------------------------------------------------
table_all <- spdf2df(samples_all,xy = F)
head(table_all)

## ------------------------------------------------------------------------
#Create data partition
set.seed(1234) #for example reproducibility
inTrain <- partition(samples_points$cluster,type="group holdout", p=0.75, groups=5,returnTrain = T)
head(inTrain)

## ------------------------------------------------------------------------
set.seed(1234)#for example reproducibility
resp.vars <- c("p95","std")
pred.vars <- setdiff(colnames(table_all),c(resp.vars))
kNN <- findNN(x = table_all[,pred.vars],y=table_all[,resp.vars],inTrain = inTrain[,1], k = 1, method = "randomForest",ntree = 200)

## ------------------------------------------------------------------------
accuracy(reference = kNN$preds$obs, estimate = kNN$preds$preds, by = kNN$preds$variable)


## ------------------------------------------------------------------------
preds_p95 <- dplyr::filter(kNN$preds,variable=="p95")
scatter(x=preds_p95$preds,y = preds_p95$obs)

preds_std <- dplyr::filter(kNN$preds,variable=="std")
scatter(x=preds_std$preds,y = preds_std$obs)

## ------------------------------------------------------------------------
imp <- varImp(kNN$model,scaled=F,plot=TRUE,plotType="boxplot")
imp$plot
imp <- varImp(kNN$model,scaled=T,plot=TRUE,plotType="grid")
imp$plot

## ------------------------------------------------------------------------
X_trgs <- stack(system.file("extdata/inputs/X_trgs/X_trgs.tif",package="foster"))
names(X_trgs) <- c("TCG_median","TCG_IQR","TCG_slope","TCW_median","TCW_IQR","TCW_slope","TCB_median","TCB_IQR","TCB_slope","NDVI_median","NDVI_IQR","NDVI_slope","DEM","DEM_slope")

X_trgs

Y_imputed <- predictTrgs(model=kNN$model,x=X_trgs,nrows = nrow(X_trgs))
Y_imputed

## ------------------------------------------------------------------------
plot(Y_imputed$p95)
plot(Y_imputed$std)
plot(Y_imputed$nnID1,col=rainbow(length(unique(Y_imputed$nnID1)))) #There are 171 unique values corresponding to 171 training samples used for imputation 

