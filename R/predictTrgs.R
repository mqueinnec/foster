#'Returns Y values at targets
#'
#'Finds the k-NN of targets and impute response variables
#'
#'By default, the data is processed row by row to avoid creating very large objects (several Gb) that couldn't be stored in memory. However, processing data row by row slows down processing. Depending on the amount of RAM available on your computer and on the size of the area where k-NN need to be calculated, it is possible to process multiple rows at the same time and considerably reduce processign times.
#'
#'
#'
#'@param model A model trained at reference locations, ususally created from \code{modelFoster}
#'@param x Predictors (X) values at targets
#'@param nrows number of rows processed at a time. Default is 1.
#'@param filename Character. Path to file written to disk (may include the extension)
#'@param par Logical. Should imputation be performed on parallel threads?
#'@param n.cores Integer. Number of parallel threads (relevant only if par=TRUE)
#'@return predicted Y variables at targets
#'@export

predictTrgs <- function(model=NULL,
                        x=NULL,
                        filename='',
                        no_cores=1,
                        nrows = 1,
                        ...){

  #Check args
  if (no_cores <= 0) stop("no_cores must be > 0")
  if (is.null(model)) stop("Need to provide trained model")
  if(is.null(x)) stop("Need to provide targets X values")
  if(!all(colnames(model$xRefs) %in% names(x))) stop("Predictor variables names don't match")

  #Define predict function
  predFun.yai <- function(model,data){
    newtrgs <- yaImpute::newtargets(model,data)
    trgs.imputed <- yaImpute::impute(newtrgs,observed=F,vars=colnames(newtrgs$yRefs))
    preds <- as.matrix(cbind(trgs.imputed,neiID=as.numeric(as.character(newtrgs$neiIdsTrgs))),rownames.force=F)
    return(preds)
  }

  #If empty filename write to temp
  if (filename=="") {
    filename <- rasterTmpFile()
  }

  #Create empty raster where predictions will be saved
  predrast <- brick(x,values=FALSE,nl=(length(colnames(model$yRefs))+1))

  #Divide x in blocks. Make it changeable
  tr <- raster::blockSize(x,minblocks = round(nrow(x)/nrows))

  #Create progress bar
  pb <- pbCreate(tr$n,label="predict",progress='text')

  if (no_cores == 1) {

    predrast <- writeStart(predrast, filename=filename, ...)

    for (i in 1:tr$n) {

      napred <- matrix(rep(NA, ncol(predrast) * tr$nrows[i] * nlayers(predrast)), ncol=nlayers(predrast))

      blockVals <- data.frame(getValues(x,row=tr$row[i],nrows=tr$nrows[i]))

      #Remove NAs from blockVals
      blockVals <- stats::na.omit(blockVals)

      if (nrow(blockVals)==0){
        predv <- napred
      } else {
        #Change rownames to avoid false duplicates with yai
        rownames(blockVals)<-paste0("trgs_",rownames(blockVals))
        #Predict
        predv <- predFun.yai(model,blockVals)
      }
      #Assign back NA
      naind <- as.vector(attr(blockVals,"na.action"))
      if(!is.null(naind)){
        p <- napred
        p[-naind,] <- predv
        predv <- p
        rm(p)
      }
      #Write preds to file
      predrast <- writeValues(predrast, predv, tr$row[i])

      raster::pbStep(pb,i)
    }


  }else{ #Parallel procesing
    .sendCall <- eval( parse( text="parallel:::sendCall") )

    #Start clusters
    beginCluster(no_cores)

    cl <- getCluster()
    on.exit(endCluster())
    nodes <- length(cl)

    #Define function that will be exectuted by clusters
    clusfun <- function(fun,i) {
      napred <- matrix(rep(NA,ncol(predrast)*tr$nrows[i]*nlayers(predrast)),ncol=nlayers(predrast))
      blockVals <- data.frame(getValues(x,row=tr$row[i],nrows=tr$nrows[i]))
      #Remove NAs from blockVals
      blockVals <- stats::na.omit(blockVals)
      if (nrow(blockVals)==0){
        predv <- napred
      } else {
        #Change rownames to avoid false duplicates with yai
        rownames(blockVals)<-paste0("trgs_",rownames(blockVals))
        #Predict
        predv <- fun(model,blockVals)
      }
      #Assign back NA
      naind <- as.vector(attr(blockVals,"na.action"))
      if(!is.null(naind)){
        p <- napred
        p[-naind,] <- predv
        predv <- p
        rm(p)
      }
      return(predv)
    }

    if(tr$n < nodes) { #If there are more nodes than blocks we don't need all clusters
      nodes <- tr$n
    }

    #Export variables that are required by clusfun
    #parallel::clusterExport(cl, c("tr","predrast","model","x"))

    #Send function to be exectuted in clusters
    for (i in 1:nodes){
      .sendCall(cl[[i]],clusfun,list(fun=predFun.yai,i=i),tag=i)
    }

    for (i in 1:tr$n) {
      pbStep(pb,i)

      d <- snow::recvOneData(cl)

      if (! d$value$success) stop("cluster error")

      if (i==1) {
        predrast <- writeStart(predrast,filename=filename,...)
      }

      predrast <- writeValues(predrast, d$value$value, tr$row[d$value$tag])
      ni <- nodes + i

      if (ni <= tr$n) {
        .sendCall(cl[[d$node]],clusfun,list(fun=predFun.yai,ni),tag=ni)
      }
    }
  }
  try(names(predrast) <- c(colnames(model$yRefs),"neiID"),silent=T)
  predrast <- writeStop(predrast)
  pbClose(pb)
  endCluster()
  return(predrast)
}
