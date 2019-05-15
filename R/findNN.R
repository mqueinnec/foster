#' Train a k-NN model
#'
#' This function trains a k-NN model from response variables (Y) and predictors (X) at reference location using the package yaImpute (see \code{\link[yaImpute]{yai}}). Optionally, training and testing sets can be provided to return the accuracy of the trained k-NN model.
#'
#' If inTest = NULL, all rows that are not in inTrain will be used for model testing. If inTrain = NULL, all rows that are not in inTest will be used for model training. If both inTrain and inTest are NULL, all rows of x and y will be used for training and no testing is performed.
#'
#' @param x A matrix or dataframe of predictors variables X for reference observations.Row names of X are used as identification of reference observations.
#' @param y A matrix or dataframe of response variables Y for the reference observations. Row names of Y are used as identification of reference observations.
#' @param inTrain Optional numeric vector indicating which rows of x and y go to training.
#' @param inTest Optional numeric vector indicating which rows of x and y go to training. If left NULL, all row that are not in inTrain are used for validation
#' @param k Integer. Number of nearest neighbors
#' @param method Character. Which nearness metrics is used to computed the nearest neighbors. Default is \code{"randomForest"}. Other methods are listed in \code{\link[yaImpute]{yai}}
#' @param ntree Number of classification or regression trees drawn for each response variable. Default is 200
#' @param mtry Number of X variables picked randomly to split each node. Default is sqrt(number of X variables)
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#'
#' @seealso \code{\link[yaImpute]{yai}}, \code{\link[yaImpute]{newtargets}}, \code{\link[yaImpute]{impute.yai}}, \code{\link[foster]{accuracy}}
#'
#' @return A list containing the following objects:
#'    \describe{
#'    \item{\code{model}}{A \code{yai} object, the trained k-NN model}
#'    \item{\code{preds}}{A data.frame with observed and predicted values of the testing set for each response variables}
#'    \item{\code{accuracy}}{A data.frame with accuracy metrics of the trained k-NN model}
#'    }
#' @export


findNN <- function(x,
                   y,
                   inTrain = NULL,
                   inTest = NULL,
                   k = 1,
                   method = "randomForest",
                   ntree = 200,
                   mtry = NULL,
                   ...) {
  if (length(k) > 1) stop("Support pny a single value of k")
  if (dim(x)[1] != dim(y)[1]) stop("x an y must have the same number of rows")


  isTest <- TRUE # Do we perform validation

  if (is.null(inTrain) & is.null(inTest)) {
    message("No training or validation set provided.")
    X_tr <- x
    Y_tr <- y
    isTest <- FALSE
  } else if (is.null(inTest)) {
    inTest <- setdiff(seq(1, dim(x)[1], 1), inTrain)
  } else if (is.null(inTrain)) {
    inTrain <- setdiff(seq(1, dim(x)[1], 1), inTest)
  }

  X_tr <- x[inTrain, ]
  Y_tr <- y[inTrain, ]

  if (isTest) {
    X_val <- x[inTest, ]
    Y_val <- y[inTest, ]
  }

  yai_object <- yaImpute::yai(x = X_tr, y = Y_tr, method = method, k = k,
                  mtry = mtry, ntree = ntree * ncol(y), bootstrap = FALSE, ...)

  if (isTest) {
    yai_newtrgs <- yaImpute::newtargets(yai_object, X_val)

    if (k == 1) {
      Y_val_predicted <- yaImpute::impute(yai_newtrgs, method = "closest",
                                          observed = T)
    } else {
      Y_val_predicted <- yaImpute::impute(yai_newtrgs, method = "dstWeighted",
                                          observed = FALSE)
    }
    Y_val_predicted <- Y_val_predicted[, colnames(Y_tr)]
    Y_val_predicted <- data.frame(ID = rownames(Y_val_predicted), Y_val_predicted)
    Y_val <- data.frame(ID = rownames(Y_val), Y_val)
    Y_pred <- reshape2::melt(Y_val_predicted, measure_vars = colnames(Y_tr),
                             value.name = "preds", id.vars = "ID")
    Y_val <- reshape2::melt(Y_val, measure.vars = colnames(Y_tr),
                            value.name = "obs", id.vars = "ID")

    preds <- merge(Y_val, Y_pred, by = c("ID", "variable"))

  } else {
    preds <- NULL
  }

  out <- list(
    model = yai_object,
    preds = preds
  )
}
