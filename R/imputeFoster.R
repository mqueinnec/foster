#'Returns Y values at targets
#'
#'@param model A model trained at reference locations, ususally created from \code{modelFoster}
#'@param Xtrgs Predictors (X) values at targets
#'@return predicted Y variables at targets

imputeFoster <- function(model=NULL,
                         Xtrgs=NULL,
                         ...){


  ###########################

  if (is.null(model)) stop("Need to provide trained model")
  if(is.null(Xtrgs)) stop("Need to provide targets X values")

  ##########################
  #Set default values

  xtrgs <- Xtrgs[complete.cases(Xtrgs),]

  if(class(model) == 'yai') {
    newtargets.model <- yaImpute::newtargets(model,newdata=xtrgs)
    Ytrgs <- yaImpute::impute(newtargets.model,observed=F)
  }

  return(Ytrgs)
}
