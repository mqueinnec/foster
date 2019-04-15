#'Calculate temporal metrics
#'
#'This function calculates a set of user-defined or default statistics from a time series of variables. If \code{s} is a Raster object, each layer should be a year of the time series. If \code{s} is a SpatialPointsDataFrame object, each column should be a year of the time series.The argument \code{fun} defines which metrics will be calculated. It has to be the name of a function that takes a vector as input and returns a named vector corresponding to the summary metrics. The function \code{defaultTemporalSummary} is used by default and returns the mean, standard deviation, median, IQR and Theil-Sen slope of the time series.
#'
#'If \code{s} is a Raster object, the processing can be parallelized using \code{\link[raster]{clusterR}}. In that case the user has to set \code{par=TRUE} and provide the number of cores \code{no-cores}.
#'
#'@param x Input Raster or SpatialPointsDataFrame object containing a time series of spectral indices (e.g. NDVI). Input stack may be generated with \code{calcIndices}
#'@param metrics Name of a function  used to process the time series.
#'@param prefix Optional. Charatcer that will be added to the names of the output
#'@param filename Filenmae if output written to disk
#'@param par Logical indication if the processigng shoudl be done on multiple cores
#'@param no_cores Number of cores to use. Only relevant if par=TRUE
#'@param m tuning parameter to determine how many blocks will be used (m blocks will be procesed by each cluster)
#'@param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#'@examples
#'#Create function that calculates mean and standard deviation and use it with temporalMetrics
#'myfunction <- function(x) c(mean=mean(x,na.rm=T),sd=sd(x,na.rm=T))
#'temporalMetrics(x,fun="myfunction")
#'@export
#'@import data.table

temporalMetrics <- function(s,
                            metrics="defaultTemporalSummary",
                            prefix=NULL,
                            filename='',
                            par=F,
                            no_cores=1,
                            m = 2,
                            ...) {

  if(is.null(prefix)) prefix <- names(s)[1]

  if(!is.character(metrics)) stop("metrics must be a character (function name)")
  if(any(grepl("\\(|\\)",metrics))) stop("metrics must be a character without brackets ()")

  eval.fun = character()
  for(f in 1:length(metrics)){
    eval.fun[f] <- paste0(metrics[f],"(value)")
  }

  eval.fun <- paste(eval.fun,collapse=",")

  if (class(s)[1]=="SpatialPointsDataFrame") {
    coordnames(s) <- c("x","y")
    ind.df <- foster::spdf2df(s,xy=T) #xy arg is not necessary for spdf, it adds a column
    ind.dt <- data.table::as.data.table(ind.df)
    #convert to long
    ind.dt.long <-  data.table::melt(ind.dt, id.vars = c("x","y"),value.name='value')
    #generate summary
    result <- ind.dt.long[,j=as.list(eval(parse(text=eval.fun))),by=list(x,y)]
    # output should be spatial object
    result <- sp::SpatialPointsDataFrame(coords=result[,c("x","y")],data=result[,-c("x","y")],proj4string = raster::crs(s))
    names(result) <- paste0(prefix,"_",names(result))

    if (filename != ''){
      rgdal::writeOGR(result,driver="ESRI Shapefile",layer=tools::file_path_sans_ext(basename(filename)),dsn=dirname(filename),...)
    }

  }else{
    if(par){
      raster::beginCluster(no_cores)
      result <- raster::clusterR(s,fun=calc,args=list(fun=eval(parse(text=metrics))),filename=filename, m=m,...,)
      raster::endCluster()
      #names(result) <- paste0(prefix,"_",names(result))
    }else{
      result <- raster::calc(s,fun=eval(parse(text=metrics)),filename=filename,...)
      names(result) <- paste0(prefix,"_",names(result))
    }
  }
  return(result)
}
