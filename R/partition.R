#' Split data into training and testing sets
#'
#' Returns the row indices of \code{x} that should go to training or validation.
#'
#' Three types of splits are currently implemented. \code{"random holdout"} randomly
#' selects \code{p} percents of \code{x} for the training set. \code{"group holdout"}
#' first groups \code{x} into \code{groups} quantiles and randomly samples
#' within them (see \code{\link[caret]{createDataPartition}}) . \code{"kfold"}
#' creates k folds where p percent of the data is used for training in each fold
#' (see \code{createFolds}). This function is a wrapper around two functions of
#' \code{caret} package: \code{\link[caret]{createDataPartition}} and
#' \code{createFolds}
#'
#' @param x A vector used for splitting data
#' @param type Character. Type of partition. Valid values are \code{"random
#'   holdout"}, \code{"group holdout"} or \code{"kfold"}
#' @param p percentage of data that goes to training set (holdout). Only
#'   relevant if \code{type = "random holdout"} or \code{type = "group holdout"}
#' @param kfold Number of folds for cross-validation. Only relevant if  \code{type =
#'   "kfold"}.
#' @param groups For \code{"group holdout"} and when x is numeric, this is the number
#'   of breaks in the quantiles
#' @param returnTrain Logical indicating whether training or validation indices
#'   should be returned. Default is TRUE.
#'
#' @seealso \code{\link[caret]{createDataPartition}}
#'
#' @return  List containing training or validation indices
#'
#' @examples
#' # sample_points is a SpatialPointsDataFrame calculated and saved from getSample
#' # Load it into memory
#' load(system.file("extdata/examples/sample_points.RData",package="foster"))
#'
#' partition(sample_points$cluster, type = "kfold", kfold = 5)
#' @export

partition <- function(x,
                      type = "group holdout",
                      p = 0.75,
                      kfold = 5,
                      groups = min(5, length(x)),
                      returnTrain = TRUE) {
  if (type == "random holdout") {
    inTrain <- list(sample(length(x), size = round(p * length(x)),
                               replace = FALSE))
  } else if (type == "group holdout") {
    inTrain <- caret::createDataPartition(x, p = p, list = TRUE,
                                          groups = groups, times = 1)
  } else if (type == "kfold") {
    inTrain <- caret::createFolds(x, k = kfold, list = TRUE, returnTrain = TRUE)
  }

  if (!returnTrain) {
    out <- lapply(inTrain, function(data, x) x[-data], x = seq(along = x))
  } else {
    out <- inTrain
  }
  return(out)
}
