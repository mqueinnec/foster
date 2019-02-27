#'Trains a kNN model from reponse variables (Y) and predictors (X) and return the accuracy of the model.
#'1. Divides the data in training and testing sets
#'2. If more than one k is given in input, performs parameter tuning using k-fold resampling
#'3. Picks the optimum value of k and train a model on all the training data
#'4. Predict values of the testing set
#'5. Compute accuracy metrics.
#'
#'@param x A dataframe of predictors variables X for reference observations.
#'row names of X are identification of observations
#'X cannot contain missing values
#'@param y A matrix or dataframe of response variables Y for the reference observations.
#'row names of Y are identification of reference observations
#'@param method Character. Type of model used for the imputation. Currently supports k-NN only
#'@param type Character. Type of distance metric ('euclidean','randomForest' etc.) if method='kNN'
#'@param k Numeric. Number of nearest neighbors
#'@param fracTrain Numeric. Fraction of observations used for training. Remaining observations go to the testing set
#'@param nFolds Numeric. During parameter tuning, k-fold resmapling is performed.
#'@param ... Additional arguments to control kNN method and type (e.g. ntree, mtry)
#'@return predicted Y variables at targets
#'@export


trainFoster <- function(x,
                        y,
                        method='kNN',
                        k=1,
                        type='randomForest',
                        ntree=200,
                        mtry=NULL,
                        fracTrain=0.75,
                        nFolds=5){

  #Check inputs, NAs in x and y etc...
  #I expect this function to return a trained model+accuracy, not perfom imputation.
  #The inputs should only be at reference location (no NAs in X nor Y)

  if(any(!is.numeric(k))) stop("k values must be numeric")
  if(any(k%%1 != 0)) stop("k values must be integers")

  #Divide in training and testing datasets
  inTrain <- createDataPartition(y[,1],p=fracTrain,list=FALSE)

  X.train <- x[inTrain,]
  Y.train <- y[inTrain,]

    resampling.summary <- data.frame()
    pred.table <- data.frame()
    #Divide training set in k folds
    folds.valid <- caret::createFolds(Y.train[,1],k=nFolds,list=TRUE,returnTrain = FALSE)
    for(p in 1:length(k)){
      message(sprintf('k=%d',p))
      for(f in 1:nFolds){
        message(names(folds.valid[f]))
        validIndex <- unlist(folds.valid[f])
        trainIndex <- unlist(folds.valid[setdiff(1:nFolds,f)])

        X.valid.set <- X.train[validIndex,]
        rownames(X.valid.set) <- validIndex
        X.train.set <- X.train[trainIndex,]
        rownames(X.train.set) <- trainIndex

        Y.valid.set <- Y.train[validIndex,]
        rownames(Y.valid.set) <- validIndex
        Y.train.set <- Y.train[trainIndex,]
        rownames(Y.train.set) <- trainIndex

        X.all <- rbind(X.train.set,X.valid.set)

        yai.model <- yaImpute::yai(x=X.all,y=Y.train.set,method=type,k=k[p],mtry=mtry,ntree=ntree*ncol(y),bootstrap = FALSE)
        if(k[p]==1){
          Y.pred <- yaImpute::impute(yai.model,method='closest',observed=F)
        }else{
          Y.pred <- yaImpute::impute(yai.model,method='dstWeighted',observed=F)
        }
        Y.pred <- Y.pred[rownames(Y.valid.set),colnames(y)]

        rmse_per <- vector()
        r2 <- vector()
        bias_per <- vector()


        for(i in 1:length(y)){
          obs <- Y.valid.set[,i]
          preds <- Y.pred[,i]

          pred.table <- rbind(pred.table,data.frame(var=colnames(y)[i],
                                                    pred=preds,
                                                    obs=obs,
                                                    rowIndex=rownames(Y.pred),
                                                    k=k[p],
                                                    Resample=names(folds.valid[f])))

        }
      }#End resampling
    }#End param tuning

    accuracy.table <- pred.table %>%
      group_by(var,k,Resample) %>%
      summarise(Rsquared=1-sum((obs-pred)^2)/sum((obs-mean(obs))^2),
                rmse = sqrt(mean((pred-obs)^2)),
                rmse_per = rmse/mean(obs)*100,
                bias = mean(pred-obs),
                bias_per = bias/mean(obs)*100)
    accuracy.table <- data.frame(accuracy.table)

    mean.per.var.k <- accuracy.table %>%
      group_by(var,k) %>%
      summarise(Rsquared_mean=mean(Rsquared),
                Rsquared.sd = sd(Rsquared),
                rmse_mean=mean(rmse),
                rmse.sd = sd(rmse),
                rmse_per_mean=mean(rmse_per),
                rmse_per.sd = sd(rmse_per),
                bias_mean = mean(bias),
                bias.sd = sd(bias),
                bias_per_mean=mean(bias),
                bias_per.sd=sd(bias_per))
    mean.per.var.k <- data.frame(mean.per.var.k)

    mean.per.k <- accuracy.table %>%
      group_by(k) %>%
      summarise(Rsquared=mean(Rsquared),
                RMSE_per=mean(rmse_per),
                bias_per=mean(bias_per))
    mean.per.k <- data.frame(mean.per.k)

    k.opt <- as.numeric(mean.per.k[which.min(mean.per.k$RMSE_per),"k"])
    message(sprintf("End of parameter tuning.\nOptimum k value is %d NN",k.opt))

    #### Testing set
    Y.test.set <- y[-inTrain,]

    yai.object.final <- yai.object <- yaImpute::yai(x=x,y=Y.train,method=type,k=k.opt,mtry=mtry,ntree = ntree*ncol(y),bootstrap = FALSE)

    if(k.opt==1){
      Y.test.preds <- yaImpute::impute(yai.object.final,method='closest',observed=F)
    }else{
      Y.test.preds <- yaImpute::impute(yai.object.final,method='dstWeighted',observed=F)
    }
    Y.test.preds <- Y.test.preds[rownames(Y.test.set),colnames(y)]

    final.pred.table <- data.frame()
    for(i in 1:length(y)){
      obs <- Y.test.set[,i]
      preds <- Y.test.preds[,i]

      final.pred.table <- rbind(final.pred.table,data.frame(var=colnames(y)[i],
                                                pred=preds,
                                                obs=obs,
                                                rowIndex=rownames(Y.test.set),
                                                k=k[p],
                                                Resample=names(folds.valid[f])))

    }

    final.model.accuracy <- final.pred.table %>%
      group_by(var) %>%
      summarise(Rsquared=1-sum((obs-pred)^2)/sum((obs-mean(obs))^2),
                rmse = sqrt(mean((pred-obs)^2)),
                rmse_per = rmse/mean(obs)*100,
                bias = mean(pred-obs),
                bias_per = bias/mean(obs)*100)
    final.model.accuracy <- data.frame(final.model.accuracy)


    out <- list(summary.tune = mean.per.var.k,
                best.tune = k.opt,
                pred=pred.table,
                resample=accuracy.table,
                finalModel=yai.object.final,
                final.model.accuracy=final.model.accuracy)
  return(out)
}

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
#
# }
