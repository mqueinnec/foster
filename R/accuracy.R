#' Calculate accuracy metrics
#'
#' Calculate coefficient of determination (R2), root-mean square error (RMSE) and bias between predictions and observations of continuous variables.
#'
#' R2 is calculated with the following formula:
#' \deqn{R^{2} = 1 - \frac{\sum (y_{i} - \hat{y}_{i})^{2}}{\sum (y_{i} - \bar{y}_{i})^{2}}}
#'
#' RMSE is calculated with the following formula:
#' \deqn{RMSE = \sqrt{\frac{1}{n} \sum (\hat{y}_{i} - y_{i})^{2}}}
#'
#' Bias is calculated with the following formula:
#' \deqn{Bias = \frac{\sum (\hat{y}_{i} - y_{i})}{n}}
#'
#' Relative RMSE and bias are also calculated by dividing their value by the mean of observations.
#'
#' If accuracy assessment was performed using k-fold cross-validation the accuracy metrics are calculated for each fold separately. The mean value of the accuracy metrics across all folds is also returned.
#'
#' @param obs A vector of observed values
#' @param preds A vector of predicted values
#' @param vars Optional vector indicating different variables
#' @param folds Optional vector indicating the folds
#'
#'@return Data frame with following columns: \describe{
#'   \item{\code{vars}}{Response variable}
#'   \item{\code{R2}}{R2}
#'   \item{\code{RMSE}}{RMSE}
#'   \item{\code{RMSE_rel}}{Relative RMSE }
#'   \item{\code{bias}}{bias}
#'   \item{\code{bias_rel}}{Relative bias}
#'   \item{\code{count}}{Number of observations}}
#' @examples
#' # kNN_preds is a data frame obtained from foster::trainNN
#' # It contains predictions and observations of the trained kNN model
#' load(system.file("extdata/examples/kNN_preds.RData",package="foster"))
#'
#' accuracy(obs = kNN_preds$obs,
#'          preds = kNN_preds$preds,
#'          vars = kNN_preds$variable,
#'          folds = kNN_preds$Fold)
#'@export
#'@importFrom dplyr %>%

accuracy <- function(obs,
                     preds,
                     vars = NULL,
                     folds = NULL){

  #check input variables
  if (!is.numeric(obs) | !is.numeric(preds)) {
    stop("Input values not numeric")
  }

  #check if input values are equal length
  if(length(obs) != length(preds)) {
    stop("Input variable not of equal length")
  }

  if(!is.null(vars)) {
    if(length(obs) != length(vars) | length(preds) != length(vars)) {
      stop("Input variable not of equal length")
    }
  }

  if(!is.null(folds)) {
    if(length(obs) != length(folds) | length(preds) != length(folds)) {
      stop("Input variable not of equal length")
    }
  }

  # Define variables as NULL (fix "no visible binding for global variable" note during check)
  RMSE_rel <- bias_rel <- count <- NULL

  # Calculate stats

  if (is.null(folds)) {
    if(is.null(vars)) {
      df <- data.frame(preds = preds, obs = obs)
      out <- df %>%
        dplyr::summarise(R2 = R2(obs = obs, preds = preds),
                         RMSE = RMSE(obs = obs, preds = preds),
                         bias = bias(obs = obs, preds = preds),
                         RMSE_rel = RMSE / mean(obs, na.rm = TRUE) * 100,
                         bias_rel = bias / mean(obs, na.rm = TRUE) * 100,
                         count = dplyr::n())
    }else{
      df <- data.frame(preds = preds, obs = obs, vars = vars)
      out <- df %>%
        dplyr::group_by(vars) %>%
        dplyr::summarise(R2 = R2(obs = obs, preds = preds),
                         RMSE = RMSE(obs = obs, preds = preds),
                         bias = bias(obs = obs, preds = preds),
                         RMSE_rel = RMSE / mean(obs, na.rm = TRUE) * 100,
                         bias_rel = bias / mean(obs, na.rm = TRUE) * 100,
                         count = dplyr::n())
    }
  }else{
    if(is.null(vars)) {
      df <- data.frame(preds = preds, obs = obs, folds = as.character(folds))
      out <- df %>%
        dplyr::group_by(folds) %>%
        dplyr::summarise(R2 = R2(obs = obs, preds = preds),
                         RMSE = RMSE(obs = obs, preds = preds),
                         bias = bias(obs = obs, preds = preds),
                         RMSE_rel = RMSE / mean(obs, na.rm = TRUE) * 100,
                         bias_rel = bias / mean(obs, na.rm = TRUE) * 100,
                         count = dplyr::n())

      out_all_folds <- out %>%
        dplyr::summarise(R2 = mean(R2),
                         RMSE = mean(RMSE),
                         bias = mean(bias),
                         RMSE_rel = mean(RMSE_rel),
                         bias_rel = mean(bias_rel),
                         count = sum(count)) %>%
        dplyr::mutate(folds = "all")

      out <- rbind(out, out_all_folds)
    }else{
      df <- data.frame(preds = preds, obs = obs, folds = as.character(folds), vars = vars)
      out <- df %>%
        dplyr::group_by(folds, vars) %>%
        dplyr::summarise(R2 = R2(obs = obs, preds = preds),
                         RMSE = RMSE(obs = obs, preds = preds),
                         bias = bias(obs = obs, preds = preds),
                         RMSE_rel = RMSE / mean(obs, na.rm = TRUE) * 100,
                         bias_rel = bias / mean(obs, na.rm = TRUE) * 100,
                         count = dplyr::n())

      nobs <- out

      out_all_folds <- out %>%
        dplyr::group_by(vars) %>%
        dplyr::summarise(R2 = mean(R2),
                       RMSE = mean(RMSE),
                       bias = mean(bias),
                       RMSE_rel = mean(RMSE_rel),
                       bias_rel = mean(bias_rel),
                       count = sum(count)) %>%
        dplyr::mutate(folds = "all")

      out <- rbind(out, out_all_folds)
    }
  }

  return(out)
}

#' Scatterplot with information on the errors between x and y.
#'
#' Scatterplot between a vector of observed data and a vector of predicted data with information on the errors between them.
#'
#' Accuracy metrics are calculated from \code{\link[foster]{accuracy}}
#'
#' @param obs A vector of observed values
#' @param preds A vector of predicted values
#' @param vars Optional vector indicating different variables
#' @param info A logical value indicating whether information on count, R2, bias and RMSE should be added to the plot
#' @return A ggplot2 object or a list of ggplot2 objects (one per variable)
#' @seealso \code{\link[foster]{accuracy}}
#' @examples
#' # kNN_preds is a data frame obtained from foster::trainNN
#' # It contains predictions and observations of the trained kNN model
#' load(system.file("extdata/examples/kNN_preds.RData",package="foster"))
#'
#' scatter(obs = kNN_preds$obs,
#'         preds = kNN_preds$preds,
#'         vars = kNN_preds$variable)
#' @export
scatter <- function (obs,
                     preds,
                     vars,
                     info = TRUE){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("'ggplot2' package is needed for this function to work.",
         call. = FALSE)
  }

  if (missing(vars)) {
      nplots = 1
      df <- data.frame(obs = obs, preds = preds)
      unique_vars <- "all"
  }else{
      unique_vars <- as.character(unique(vars))
      nplots <- length(unique_vars)
      df <- data.frame(obs = obs, preds = preds, vars = vars)
  }

  out_plot <- list()

  for (n in unique_vars) {
    if (!missing(vars)) {
      data <- dplyr::filter(df, vars == n)
    }else{
      data <- df
    }

      if (info == TRUE) {
        d <- accuracy(obs = data$obs, preds = data$preds)

        label <- paste("n", " = ", d$count, "\n",
                       "R2", " = ", round(d$R2,2), "\n",
                       "RMSE", " = ", round(d$RMSE, 3), "\n",
                       "RMSE%", " = ", round(d$RMSE_rel, 2), "\n",
                       "bias", " = ", round(d$bias, 3), "\n",
                       "bias%", " = ", round(d$bias_rel, 2), "\n",
                       sep = "")

        lowerlimit <- min(data[c("obs", "preds")], na.rm = TRUE)
        upperlimit <- max(data[c("obs", "preds")], na.rm = TRUE)

        if (is.finite(d$bias_rel) & d$bias_rel < -20) {
          ann_x <- lowerlimit
          ann_y <- upperlimit
          ann_hjust <- 0
          ann_vjust <- 0.9
        }else{
          ann_x <- upperlimit
          ann_y <- -Inf
          ann_hjust <- 1
          ann_vjust <- -0.2
        }

        ann <- ggplot2::annotate("text",
                                 x = ann_x,
                                 y = ann_y,
                                 label = label,
                                 hjust = ann_hjust,
                                 vjust = ann_vjust)

      }

    if (info == TRUE) {
      out_plot[[n]] <- ggplot2::ggplot(data = data,
                                                    ggplot2::aes(x = preds, y = obs)) +
        ggplot2::geom_point(shape = 1, size = 2) +
        ggplot2::coord_equal(ratio = 1) +
        ggplot2::xlab("Predicted") +
        ggplot2::ylab("Observed") +
        ann +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }else{
      out_plot[[n]] <- ggplot2::ggplot(data = data,
                                       ggplot2::aes(x = preds, y = obs)) +
        ggplot2::geom_point(shape = 1, size = 2) +
        ggplot2::coord_equal(ratio = 1) +
        ggplot2::xlab("Predicted") +
        ggplot2::ylab("Observed") +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    }

  }
  if (nplots == 1) {
    return(out_plot[[1]])
  }else{
    return(out_plot)
  }

}
