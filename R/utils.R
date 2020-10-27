#' Default temporal summary
#'
#' Calculates median, IQR and Theil Sen slope (\code{\link[trend]{sens.slope}}).
#' This function is usually called within \code{\link[foster]{temporalMetrics}}
#'
#'
#' @param x Vector of numeric values
#' @return Named vector with median, IQR and slope
#' @seealso \code{\link[foster]{temporalMetrics}}, \code{\link[trend]{sens.slope}}
#' @examples
#' x <- rnorm(100)
#' defaultTemporalSummary(x)
#' @export

defaultTemporalSummary <- function(x) {
  c(
    median = stats::median(x, na.rm = TRUE),
    IQR = stats::IQR(x, na.rm = TRUE),
    slope = theilSen(x)
  )
}

#' Theil-Sen slope
#'
#' Calculate the Theil-Sen slope from a time series. This is a wrapper around \code{\link[trend]{sens.slope}}
#'
#' @param x A numeric vector
#' @return numeric; Theil-Sen slope
#' @seealso \code{\link[trend]{sens.slope}}
#' @examples
#' x <- rnorm(100)
#' theilSen(x)
#' @export

theilSen <- function(x) {

  if (all(is.na(x))){
    return(NA)
  }else{
    if (any(is.na(x))) {
      warning("NA values in time series")
      x <- x[!is.na(x)]
    }
    return(as.numeric(trend:: sens.slope(x)[1]))
  }

}

#' Convert a SpatialPointsDataFrame to a data.frame
#'
#' Convert a SpatialPointsDataFrame to a data.frame. Coordinates of \code{x} can be added to the data.frame using \code{xy = TRUE}
#'
#' @param x A \code{\link[sp]{SpatialPoints}} object
#' @param xy Logical. If TRUE, coordinates of \code{x} are added to data.frame
#' @return data.frame
#' @examples
#' # sample_points is a SpatialPointsDataFrame calculated and saved from getSample
#' # Load it into memory
#' load(system.file("extdata/examples/sample_points.RData",package="foster"))
#'
#' sample_df <- foster:::spdf2df(sample_points)
#' @noRd

spdf2df <- function(x, xy = FALSE) {
  out <- raster::as.data.frame(x)
  if (!xy) {
    coord.name <- sp::coordnames(x)
    out[, coord.name[1]] <- NULL
    out[, coord.name[2]] <- NULL
  }
  return(out)
}

#' Coefficient of determination
#'
#' Calculate coefficient of determination from observations and predictions
#'
#' R2 is calculated with the following formula:
#' \deqn{R^{2} = 1 - \frac{\sum (y_{i} - \hat{y}_{i})^{2}}{\sum (y_{i} - \bar{y}_{i})^{2}}}
#'
#'
#' @param obs Vector of observed values
#' @param preds Vector of predicted values
#'
#' @return numeric
#' @examples
#' x <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' y <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' foster:::R2(x, y)
#' @noRd

R2 <- function(obs,
               preds) {

  to_rm <- is.na(obs) | is.na(preds)
  obs <- obs[!to_rm]
  preds <- preds[!to_rm]

  1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
}

#' Root mean square error
#'
#' Calculate root mean square error (RMSE) from observations and predictions
#'
#' RMSE is calculated with the following formula:
#' \deqn{RMSE = \sqrt{\frac{1}{n} \sum (\hat{y}_{i} - y_{i})^{2}}}
#'
#' @param obs Vector of observations
#' @param preds Vector of predictions
#'
#' @return numeric
#' @examples
#' x <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' y <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' foster:::RMSE(x, y)
#' @noRd

RMSE <- function(obs,
               preds) {

  sqrt(mean((preds - obs)^2, na.rm = TRUE))
}

#' Bias
#'
#' Calculate bias between observations and predictions
#'
#' Bias is calculated with the following formula:
#' \deqn{Bias = \frac{\sum (\hat{y}_{i} - y_{i})}{n}}
#'
#' @param obs Vector of observations
#' @param preds Vector of predictions
#'
#' @return numeric
#' @examples
#' x <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' y <- seq(1,100,1)*rnorm(100,mean = 1,sd = 0.1)
#' foster:::bias(x, y)
#' @noRd

bias <- function(obs,
                 preds) {

  mean(preds - obs, na.rm = TRUE)
}



