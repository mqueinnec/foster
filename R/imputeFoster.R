#'Returns Y values at targets
#'
#'@param model A model trained at reference locations, ususally created from \code{modelFoster}
#'@param Xtrgs Predictors (X) values at targets
#'@param filename Character. Path to file written to disk (may include the extension)
#'@param par Logical. Should imputation be performed on parallel threads?
#'@param n.cores Integer. Number of parallel threads (relevant only if par=TRUE)
#'@return predicted Y variables at targets
#'@export

imputeFoster <- function(model=NULL,
                         X.trgs=NULL,
                         filename='',
                         par=F,
                         n.cores=1,
                         ...){

  #Define predict function
  predFun.yai <- function(model,data){
    rownames(data)<-paste0("trgs_",rownames(data)) ## so that newtargets doesn't get confused by duplicate row IDs.
    newtrgs <- yaImpute::newtargets(model,data)
    trgs.imputed <- yaImpute::impute(newtrgs,observed=F,vars=colnames(newtrgs$yRefs))
    return(as.matrix(cbind(trgs.imputed,neiID=as.numeric(as.character(newtrgs$neiIdsTrgs))),rownames.force=F))
  }


  ###########################

  if (is.null(model)) stop("Need to provide trained model")
  if(is.null(X.trgs)) stop("Need to provide targets X values")

  # if(class(X.trgs)[1] %in% c("RasterLayer","RasterStack","RasterBrick")){
  #   coords.trgs <- data.frame(sp::coordinates(X.trgs))
  #   Xtrgs <- raster::as.data.frame(X.trgs)
  # }

  #rownames(Xtrgs) <- paste0("trgs_",rownames(Xtrgs))

  #Xtrgs.noNA <- complete.cases(Xtrgs)
  #out <- data.frame(coords.trgs)

  ##########################
  #Set default values
  if(!all(colnames(model$xRefs) %in% names(X.trgs))) stop("Predictor variables names don't match")

  if(par){
    raster::beginCluster(n=n.cores)
    raster::clusterR(X.trgs,predict,args=list(model=model,fun=predFun.yai,na.rm=T,index=1:(length(colnames(model$yRefs))+1)),filename=filename,progress='text',m=2)
    raster::endCluster()
  }else{
    raster::predict(object=X.trgs,model=model,fun=predFun.yai,na.rm=T,index=1:(length(colnames(model$yRefs))+1),filename=filename,progress='text')
  }
  #Xtrgs <- Xtrgs[Xtrgs.noNA,]


  # if(class(model) == 'yai') {
  #   newtargets.model <- yaImpute::newtargets(model,newdata=Xtrgs)
  #   Ytrgs <- yaImpute::impute(newtargets.model,observed=F,vars=colnames(newtargets.model$yRefs))
  # }

  #Ytrgs <- Ytrgs[,colnames(newtargets.model$yRefs)]

  # #Create a raster stack filled with Ytrgs values
  # if(class(X.trgs)[1] %in% c("RasterLayer","RasterStack","RasterBrick")){
  #   for(i in 1:length(Ytrgs)){
  #     out <- eval(parse(text=paste0("data.frame(out,",colnames(Ytrgs)[i],"=NA)")))
  #     out[Xtrgs.noNA,colnames(Ytrgs)[i]] <- Ytrgs[,colnames(Ytrgs)[i]]
  #   }
  #   coordinates(out) <- ~x+y
  #   gridded(out) <- TRUE
  #
  #   out <- raster::stack(out)
  #   crs(out) <- crs(X.trgs)
  #
  #   return(out)
  # }else{
  #   return(Ytrgs)
  # }

}
