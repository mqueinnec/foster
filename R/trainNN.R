#' Train and assess accuracy of a k-NN model
#'
#' This function trains a k-NN model from response variables (Y) and predictors
#' (X) at reference observations using the package yaImpute (see
#' \code{\link[yaImpute]{yai}}). By default, the distance between observations
#' is obtained from the proximity matrix of random forest regression or
#' classification trees. Optionally, training and testing sets can be provided
#' to return the accuracy of the trained k-NN model.
#'
#' If performing model validation, the function trains a kNN model from the
#' training set, finds the k NN of the validation set and imputes the response
#' variables from the k NN. If \code{k = 1}, only the closest NN value is
#' imputed. If k > 1, the imputed value can be either the closest NN value, the
#' mean, median or distance weighted mean of the k NN values.This is controlled
#' by the arguments \code{impute.cont} or \code{impute.fac}.
#'
#' If inTest = NULL, all rows that are not in inTrain will be used for model
#' testing. If inTrain = NULL, all rows that are not in inTest will be used for
#' model training. If both inTrain and inTest are NULL, all rows of x and y will
#' be used for training and no testing is performed.
#'
#' The final model returned by \code{findNN} is trained from all observations of
#' \code{x} and \code{y}.
#'
#'@param x A dataframe or SpatialPointsDataFrame of predictors variables X for
#'  reference observations. Row names of X are used as identification of
#'  reference observations.
#'@param y A dataframe or SpatialPointsDataFrame of response variables Y for the
#'  reference observations. Row names of Y are used as identification of
#'  reference observations.
#'@param inTrain Optional. A list obtained from
#'  \code{\link[foster]{partition}}indicating which rows of x and y go to
#'  training.
#'@param inTest Optional list indicating which rows of x and y go to validation.
#'  If left NULL, all rows that are not in \code{inTrain} are used for
#'  validation.
#'@param k Integer. Number of nearest neighbors
#'@param method Character. Which nearness metrics is used to compute the nearest
#'  neighbors. Default is \code{"randomForest"}. Other methods are listed in
#'  \code{\link[yaImpute]{yai}}
#'@param impute.cont Character. The method used to compute the imputed
#'  continuous variables. Can be \code{"closest"}, \code{"mean"},
#'  \code{"median"} or \code{"dstWeighted"}. Default is \code{"closest"} if
#'  \code{k = 1} and \code{"dstWeighted"} if \code{k > 1}. See
#'  \code{\link[yaImpute]{impute.yai}} for more details.
#'@param impute.fac Character. The method used to compute the imputed values for
#'  factors. Default value is the same as \code{impute.cont}. See
#'  \code{\link[yaImpute]{impute.yai}} for more details.
#'@param ntree Number of classification or regression trees drawn for each
#'  response variable. Default is 500
#'@param mtry Number of X variables picked randomly to split each node. Default
#'  is sqrt(number of X variables)
#'@param rfMode By default, \code{rfMode} is set to \code{""} which forces
#'  \code{\link[yaImpute]{yai}} to create random forest regression trees instead
#'  of classification trees for continuous variables. Can be set to
#'  \code{"buildClasses"} if  wanting continuous variables to be converted to
#'  classes and forcing random forest to build classification trees. (See
#'  \code{\link[yaImpute]{yai}})
#'@param ... Other arguments passed to \code{\link[yaImpute]{yai}} (e.g.
#'  \code{"rfXsubsets"})
#'
#'@return A list containing the following objects: \describe{
#'   \item{\code{model}}{A \code{yai} object, the trained k-NN model}
#'   \item{\code{preds}}{A data.frame with observed and predicted values of the
#'   testing set for each response variables} }
#'
#'@seealso \code{\link[yaImpute]{yai}}, \code{\link[yaImpute]{newtargets}},
#'   \code{\link[yaImpute]{impute.yai}}, \code{\link[foster]{accuracy}}
#'
#' @examples
#' # Load data in memory
#' # X_vars_sample: Predictor variables at sample (from getSample)
#' # Y_vars_sample: Response variables at sample (from getSample)
#' # train_idx: Rows of X_vars_sample and Y_vars_sample that are used for
#' # training (from (partition))
#' load(system.file("extdata/examples/example_trainNN.RData",package="foster"))
#'
#' set.seed(1234) #for example reproducibility
#' kNN <- trainNN(x = X_vars_sample,
#'                y=Y_vars_sample,
#'                inTrain = train_idx,
#'                k = 1,
#'                method = "randomForest",
#'                ntree = 200)
#' @export


trainNN <- function(x,
                   y,
                   inTrain = NULL,
                   inTest = NULL,
                   k = 1,
                   method = "randomForest",
                   impute.cont = NULL,
                   impute.fac = NULL,
                   ntree = 500,
                   mtry = NULL,
                   rfMode = "",
                   ...) {

  if (length(k) > 1) stop("Support only a single k value")
  if (dim(x)[1] != dim(y)[1]) stop("x an y must have the same number of rows")

  if (class(x) %in% "SpatialPointsDataFrame") {
    x <- spdf2df(x)
  }

  if (class(y) %in% "SpatialPointsDataFrame") {
    y <- spdf2df(y)
  }

  if (is.null(inTrain) & is.null(inTest)) {
    message("No training or validation set provided.")
    isTest <- FALSE
  }

  if(!is.null(inTrain)) {
    isTest <- TRUE
    if(is.list(inTrain)) {
      nfolds_train <- length(inTrain)
    }else{
      nfolds_train <- 1
      inTrain <- list(inTrain)
    }
  }

  if(!is.null(inTest)) {
    isTest <- TRUE
    if(is.list(inTest)) {
      nfolds_test <- length(inTest)
    }else{
      nfolds_test <- 1
      inTest <- list(inTest)
    }
  }

  if (is.null(inTrain)) {
    nfolds_train <- 0
  }
  if (is.null(inTest)) {
    nfolds_test <- 0
  }

  if (nfolds_train > nfolds_test &!is.null(inTest)) {
    inTest <- NULL
    message("Performing cross-validation: inTest argument ignored. Test samples are determined from the supplied training folds.")
  }

  if (nfolds_train < nfolds_test &!is.null(inTrain)) {
    inTrain <- NULL
    message("Performing cross-validation: inTrain argument ignored. Validation samples are determined from the supplied testing folds.")
  }

  # Number of folds is either 1 or the max number of folds in either test or training
  nfolds <- max(c(nfolds_train, nfolds_test))

  # Set rules for imputation
  if (k == 1) {
    impute.cont <- "closest"
    impute.fac <- "closest"
  } else {
    if (is.null(impute.cont)) impute.cont <- "dstWeighted"
    if (is.null(impute.fac)) impute.fac <- impute.cont
  }

  if (isTest) {
    # List storing all predictions and validation
    preds_out <- list()

    for (n in 1:nfolds) {

      if (is.null(inTest)) {
        train_fold <- inTrain[[n]]
        test_fold <- setdiff(seq(1, dim(x)[1], 1), train_fold)
      }else if (is.null(inTrain)) {
        test_fold <- inTest[[n]]
        train_fold <- setdiff(seq(1, dim(x)[1], 1), test_fold)
      }else{
        test_fold <- inTest[[n]]
        train_fold <- inTrain[[n]]
      }

      # Make sure than row indices of train_fold and test_fold are valid
      if(any(!c(test_fold,train_fold) %in% seq_len(dim(x)[1]))) {
        stop("Invalid training or validation row indices in inTrain or inTest")
      }

      if(anyDuplicated(c(test_fold,train_fold)) != 0) {
        warning(sprintf("Fold %d: duplicated row indices in training and validation", n))
      }

      X_tr <- x[train_fold, ]
      Y_tr <- y[train_fold, ]

      X_val <- x[test_fold, ]
      Y_val <- y[test_fold, ]



      #Train yai
      yai_object <- yaImpute::yai(x = X_tr,
                                  y = Y_tr,
                                  method = method,
                                  k = k,
                                  mtry = mtry,
                                  ntree = ntree * ncol(y),
                                  rfMode = rfMode,
                                  ...)

      # Find NN and impute at validation
      yai_newtrgs <- yaImpute::newtargets(yai_object, X_val)

      Y_val_predicted <- yaImpute::impute(yai_newtrgs,
                                          method = impute.cont,
                                          method.factor = impute.fac,
                                          observed = FALSE)

      Y_val_predicted <- Y_val_predicted[, colnames(Y_tr)]
      Y_val_predicted <- data.frame(ID = rownames(Y_val_predicted), Y_val_predicted)
      Y_val <- data.frame(ID = rownames(Y_val), Y_val)
      Y_pred <- reshape2::melt(Y_val_predicted, measure_vars = colnames(Y_tr),
                               value.name = "preds", id.vars = "ID")
      Y_val <- reshape2::melt(Y_val, measure.vars = colnames(Y_tr),
                              value.name = "obs", id.vars = "ID")

      preds <- merge(Y_val, Y_pred, by = c("ID", "variable"))
      # Keep track of fold
      preds$Fold <- n

      preds_out[[n]] <- preds

    }
    preds_out <- do.call(rbind, preds_out)
  }else{ #No testing/validation
    preds_out <- NULL
    # Once validation is done, the final model is trained with all observations

  }

  yai_object_final <- yaImpute::yai(x = x,
                                    y = y,
                                    method = method,
                                    k = k,
                                    mtry = mtry,
                                    ntree = ntree * ncol(y),
                                    rfMode = rfMode,
                                    ...)

  # Add imputation mode to be used with predictTrgs
  yai_object_final$impute.cont <- impute.cont
  yai_object_final$impute.fac <- impute.fac

  out <- list(
    model = yai_object_final,
    preds = preds_out
  )

  return(out)
}
