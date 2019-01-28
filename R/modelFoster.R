#'Train a kNN (or regression model) from reponse variables (Y) and predictors (X) at reference location
#'
#'@param x A matrix or dataframe of predictors variables X for reference observations.Row names of X are identification of observations. X cannot contain missing values
#'@param y A matrix or dataframe of response variables Y for the reference observations. Row names of Y are identification of reference observations
#'@param inTrain Optional numeric vector indicating which rows of x and y go to training.
#'@param inVal Optional numeric vector indicating which rows of x and y go to training. If left empy, all row that are not in inTrain are used for validation
#'@param k Integer. Number of nearest neighbors
#'@param method Character. Which nearness netrics is used to computed the nearest neighbors. Default is \code{"randomForest"}. Other methods are listed in \code{\link[yaImpute]{yai}}
#'@param ntree Number of classification or regression trees drawn for each response variable. Default is 200
#'@param mtry Number of X varaibles picked randomly to split each node. Default is sqrt(number of X variables)
#'@return predicted Y variables at targets


modelFoster <- function(x,
                        y,
                        inTrain=NULL,
                        inVal=NULL,
                        k=1,
                        method='randomForest',
                        ntree=200,
                        mtry=NULL
                        ){

  if(length(k)>1) stop("More than one k value. Parameter tuning is not yet implemented")

  isVal <- TRUE #Do we perform validation

  if(is.null(inTrain) & is.null(inVal)) {
    message("No training or validation set provided.")
    X.tr <- x
    Y.tr <- y
    isVal <- TRUE
  }else if(is.null(inVal)){
    inVal <- setdiff(seq(1,dim(x)[1],1),inTrain)
  }else if(is.null(inTrain)){
    inTrain <- setdiff(seq(1,dim(x)[1],1),inVal)
  }

  X.tr <- x[inTrain,]
  Y.tr <- y[inTrain,]

  if(isVal){
    X.val <- x[inVal,]
    Y.val <- y[inVal,]
  }

  yai.object <- yaImpute::yai(x=X.tr,y=Y.tr,method=method,k=k,mtry=mtry,ntree = ntree*ncol(y),bootstrap = FALSE)

  if(isVal){
    yai.newtrgs <- yaImpute::newtargets(yai.object,X.val)

    if (k==1){
      Y.val.predicted <- yaImpute::impute(yai.newtrgs,method='closest',observed=FALSE)
    }else{
      Y.val.predicted <- yaImpute::impute(yai.newtrgs,method='dstWeighted',observed=FALSE)
    }
    Y.val.predicted <- Y.val.predicted[,colnames(Y.tr)]
    Y.val.predicted <- data.frame(ID=rownames(Y.val.predicted),Y.val.predicted)
    Y.val <- data.frame(ID=rownames(Y.val),Y.val)
    Y.pred <- reshape2::melt(Y.val.predicted,measure.vars=colnames(Y.tr),value.name='preds')
    Y.val <- reshape2::melt(Y.val,measure.vars=colnames(Y.tr),value.name="obs")

    preds <- merge(Y.val,Y.pred,by=c('ID','variable'))

    accTable <- foster::accuracyFoster(reference=preds$obs,estimate = preds$preds,by=preds$variable)

  }else{
    preds = NULL
  }

  out <- list(
    kNN.model = yai.object,
    preds = preds,
    accuracy = accTable
  )
}










#   #Check inputs, NAs in x and y etc...
#   #I expect this function to return a trained model, not perfom imputation.
#   #The inputs should only be at reference location, not targets
#
#   theStats = NULL
#
# if(cv=="holdout"){
#   if(is.na(fracTrain)|is.null(fracTrain)|!is.numeric(fracTrain)) stop("Not valid fracTrain")
#   if(fracTrain>1 | fracTrain < 0) stop("fracTrain needs to be between 0 and 1")
#   trainIndex <- sample(rownames(x),round(fracTrain*nrow(x)))
#   validIndex <- setdiff(rownames(x),trainIndex)
#
#   X.tr <- x[trainIndex,]
#   Y.tr <- y[trainIndex,]
#
#   X.val <- x[validIndex,]
#   Y.val <- y[validIndex,]
#
#   if(method=='kNN'){
#     #Train the kNN model
#     yai.object <- yaImpute::yai(x=X.tr,y=Y.tr,method=type,k=k,mtry=mtry,ntree = ntree*ncol(y),bootstrap = FALSE)
#     #Find k-NN of validation
#     newtargets.output <- yaImpute::newtargets(yai.object,X.val)
#     if (k==1){
#       Y.val.predicted <- yaImpute::impute(newtargets.output,method='closest',observed=FALSE)
#     }else{
#       Y.val.predicted <- yaImpute::impute(newtargets.output,method='dstWeighted',observed=FALSE)
#     }
#     Y.val.predicted <- Y.val.predicted[,colnames(Y.tr)]
#
#     model_cv <- yai.object
#     #Get accuracy
#     obs <- melt(Y.val,measure.vars=colnames(y))
#     preds <- melt(Y.val.predicted,measure.vars=colnames(y))
#     theStats <- accuracyFoster(obs$value,preds$value,by=obs$variable)
#
#     cv_summary <- list(val.fit = data.frame(var=obs$variable,val.obs=obs$value,val.preds=preds$value),trainIndex=trainIndex,validIndex=validIndex)
#   }else{
#     stop("Currently only kNN is implemented")
#   }
#
# }else if(cv=="loocv"){
#   #Will store each the predictions of each loop (1 prediction per loop)
#   Y.val.predicted = NULL
#
#   for(i in 1:nrow(x)){
#     if(i%%10 == 0) print(sprintf("%d%%",round(i/nrow(x)*100)))
#     trainIndex <- rownames(x[-i,])
#     validIndex <- rownames(x[i,])
#
#     X.tr <- x[trainIndex,]
#     Y.tr <- y[trainIndex,]
#
#     X.val <- x[validIndex,]
#     Y.val <- y[validIndex,]
#
#     if(method=='kNN'){
#       #Train the kNN model
#       yai.object <- yaImpute::yai(x=X.tr,y=Y.tr,method=type,k=k,mtry=mtry,ntree = ntree*ncol(y),bootstrap = FALSE)
#       #Find k-NN of validation
#       newtargets.output <- yaImpute::newtargets(yai.object,X.val)
#       if (k==1){
#         Y.val.predicted_temp <- yaImpute::impute(newtargets.output,method='closest',observed=FALSE)
#       }else{
#         Y.val.predicted_temp <- yaImpute::impute(newtargets.output,method='dstWeighted',observed=FALSE)
#       }
#       Y.val.predicted <- rbind(Y.val.predicted,Y.val.predicted_temp[,colnames(Y.tr)])
#     }
#
#   }
#
#   model_cv <- yai.object
#
#   obs <- melt(Y.val,measure.vars=colnames(y))
#   preds <- melt(Y.val.predicted,measure.vars=colnames(y))
#   theStats <- accuracyFoster(obs$value,preds$value,by=obs$variable)
#
# }else if(cv=="kfold"){
#
#   if(is.na(folds)|is.null(folds)|!is.numeric(folds)|!(folds%%1 == 0)) stop("Need to provide a valid number of folds for k-fold CV")
#   #Will store each the predictions of each loop (1 prediction per loop)
#
#   #Calculate the size of each fold
#   foldIndex <- seq(1,folds,1)
#   nFolds <- rep(floor(nrow(x)/folds),folds)
#   addObs <- nrow(x) - sum(nFolds)
#   #Randomly add obsevrations to match # observations (floor is used above)
#   if(addObs != 0) {
#     for (i in 1:addObs){
#       n <- sample(foldIndex,1)
#       nFolds[n] <- nFolds[n]+1
#     }
#   }
#
#    trainIndex =NULL
#    validIndex = NULL
#    candidates <- rownames(x)
#
#    Y.val.predicted = NULL
#    theStats = NULL
#    model_cv = NULL
#    cv_summary = NULL
#   for(i in 1:folds){
#     print(sprintf("Doing fold %d",i))
#
#     #fold[i] is the validation sample, rest is training
#     validIndex[[i]] <- sample(candidates,nFolds[i],replace=FALSE)
#     trainIndex[[i]] <- setdiff(rownames(x),validIndex[[i]])
#
#     #Remove row that are already in a fold
#     candidates <- setdiff(candidates,unlist(validIndex))
#
#     X.tr <- x[trainIndex[[i]],]
#     Y.tr <- y[trainIndex[[i]],]
#
#     X.val <- x[validIndex[[i]],]
#     Y.val <- y[validIndex[[i]],]
#
#     if(method=='kNN'){
#       #Train the kNN model
#       yai.object <- yaImpute::yai(x=X.tr,y=Y.tr,method=type,k=k,mtry=mtry,ntree = ntree*ncol(y),bootstrap = FALSE)
#       #Find k-NN of validation
#       newtargets.output <- yaImpute::newtargets(yai.object,X.val)
#       if (k==1){
#         Y.val.predicted <- yaImpute::impute(newtargets.output,method='closest',observed=FALSE)
#       }else{
#         Y.val.predicted<- yaImpute::impute(newtargets.output,method='dstWeighted',observed=FALSE)
#       }
#       Y.val.predicted <- Y.val.predicted[,colnames(Y.tr)]
#       Y.val.predicted <- Y.val.predicted[rownames(Y.val),]
#       obs[[i]] <- melt(Y.val,measure.vars=colnames(y))
#       preds[[i]] <- melt(Y.val.predicted,measure.vars=colnames(y))
#       theStats_kfold <- accuracyFoster(obs$value,preds$value,by=obs$variable)
#
#       theStats <- rbind(theStats,data.frame(fold=i,theStats_kfold))
#       model_cv[[i]] <- yai.object
#
#       cv_summary[[i]] <- list(Fold=i,val.fit = data.frame(val.obs=obs,val.preds=preds),trainIndex=trainIndex,validIndex=validIndex)
#     }
#   }
#    theStats <- dplyr::arrange(theStats,factor)
#
#
# }
#
#   if(cv=='none'|finalModel=='All'){
#     #Train the final model on the entire dataset
#     X.tr <- x
#     Y.tr <- y
#
#     if (method=='kNN'){
#       yai.object <- yaImpute::yai(x=x,y=y,method=type,k=k,mtry=mtry,ntre=ntree*ncol(y),bootstrap=FALSE)
#     }
#     model_cv <- yai.object
#   }
#
#   model.final <- model_cv
#   #Return the kNN or regression model and stats associated to the cross validation
#   return(list(model=model.final,crossValidationType=cv,accuracy = theStats,cv_summary=cv_summary))


