#'Returns Y values at targets
#'
#'@param model A model trained at reference locations, ususally created from \code{modelFoster}
#'@param Xtrgs Predictors (X) values at targets
#'@return predicted Y variables at targets

imputeFoster <- function(model=NULL,
                         X.trgs=NULL,
                         ...){


  ###########################

  if (is.null(model)) stop("Need to provide trained model")
  if(is.null(X.trgs)) stop("Need to provide targets X values")

  if(class(X.trgs)[1] %in% c("RasterLayer","RasterStack","RasterBrick")){
    coords.trgs <- data.frame(sp::coordinates(X.trgs))
    Xtrgs <- raster::as.data.frame(X.trgs)
  }

  rownames(Xtrgs) <- paste0("trgs_",rownames(Xtrgs))

  Xtrgs.noNA <- complete.cases(Xtrgs)
  out <- data.frame(coords.trgs)

  ##########################
  #Set default values

  Xtrgs <- Xtrgs[Xtrgs.noNA,]

  if(class(model) == 'yai') {
    newtargets.model <- yaImpute::newtargets(model,newdata=Xtrgs)
    Ytrgs <- yaImpute::impute(newtargets.model,observed=F)
  }

  Ytrgs <- Ytrgs[,colnames(newtargets.model$yRefs)]

  #Create a raster stack filled with Ytrgs values
  if(class(X.trgs)[1] %in% c("RasterLayer","RasterStack","RasterBrick")){
    for(i in 1:length(Ytrgs)){
      out <- eval(parse(text=paste0("data.frame(out,",colnames(Ytrgs)[i],"=NA)")))
      out[Xtrgs.noNA,colnames(Ytrgs)[i]] <- Ytrgs[,colnames(Ytrgs)[i]]
    }
    coordinates(out) <- ~x+y
    gridded(out) <- TRUE

    out <- raster::stack(out)
    crs(out) <- crs(X.trgs)

    return(out)
  }else{
    return(Ytrgs)
  }

}
