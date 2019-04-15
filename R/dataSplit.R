#'Split data into training and testing sets
#'
#'Returns the indices of \code{x} that should go to trainign or testing
#'
#'Three types of splits are currently implemented. "random holdout" randomly select \code{p} percents of \code{x} for the training set. 'group holdout" first group \code{x} into \code{groups} quantiles and randomly samples within them. "kfold" creates k folds where p percent of the data is used for training in each fold.
#'
#'@param x A vector used for splitting data
#'@param type Character. How should data be splitted? Valid values are "random holdout" , "group holdout" or "kfold"
#'@param p percentage of data that goes to training set (holdout) or to each fold (1/k)
#'@param groups For "group holdout" and when x is numeric, this is the number of breaks in the quantiles
#'@param returnTrain Logical indicating whether training data or testing data should be returned
#'@export

dataSplit <- function(x,
                      type="group holdout",
                      p = 0.75,
                      groups = min(5,length(x)),
                      returnTrain=TRUE) {

  if(type == "random holdout"){
    inTrain <- train=sample(length(x),size=round(p*length(x)),replace=FALSE)
  }else if (type=="group holdout"){
    inTrain <- caret::createDataPartition(x,p=p,list=FALSE,groups=groups)
  }else if (type == "kfold") {
    k = round(1/(1-p),digits=0)
    inTrain <- caret::createFolds(x,k=k,list=T,returnTrain = TRUE)
  }

  if(!returnTrain) {
    out <- lapply(inTrain,function(data,x) x[-data], x = seq(along=x))
  }else{
    out <- inTrain
  }
  return(out)
}
