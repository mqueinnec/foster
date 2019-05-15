#' Calculate accuracy metrics of two vectors
#'
#' This is a direct call to \code{\link[scatter]{calc.error}}. Calculate bias, RMSE, correlation etc of two vectors.
#'
#' @param reference a vector of reference values
#' @param estimate a vector of estimated values
#' @param by Optional grouping variable
#' @param noinfo Logical. Should the additional information on the calculations be displayed?
#' @param dist.normal Logical. Is the distribution normal? If TRUE t-test and Pearson correlation is calculated. If FALSE, Wilcoxon paired test and Spearman correlation is calculated.
#' @param short Logical. Calculate only a subset of summary statistics.
#' @return summary statistics of the differences between \code{reference} and \code{estimate}.
#'@references \url{https://github.com/ptompalski/scatter}
#'@export

accuracy <- function(reference,
                     estimate,
                     by = NULL,
                     noinfo = TRUE,
                     dist.normal = TRUE,
                     short = FALSE){

  scatter::calc.error(reference, estimate, by, noinfo, dist.normal, short)
}

#' scatterplot with information on the errors between x and y.
#'
#' This is a direct call to \code{\link[scatter]{scatter}}.This scatterplot is a wrapper function for a ggplot-based plot. It contains additional text panel that shows values calculated with \code{\link{calc.error}}
#'
#' @param x a vector of observed data.
#' @param y a vector of predicted data.
#' @param R2 Optional. Can be enabled or disabled by setting TRUE/FALSE. Can also be a value in cases where the R2 is calculated by other function.
#' @param axisorder Optional. Set to \code{PO} (predicted-observed) to plot predicted (\code{y}) on the y-axis (this is the default). Set to \code{OP} (observed-predicted) to plot observed (\code{x}) on the y-axis.
#' @param xlab Optional. Title of the x-axis
#' @param ylab Optional. Title of the y-axis
#' @param info A logical value indicating whether information on count, bias and RMSE should be added to the plot.
#' @param position Determines the position of the info box
#' @param positionauto A logical value indicating whether the position of the info box should be optimized automatically.
#' @param lowerlimit A value determining the lower limit of the x and y axis
#' @param upperlimit A value determining the upper limit of the x and y axis
#' @param alpha Define the transparency of the points. 0 - fully transparent, 1 - opaque.
#' @param add.reg.line Logical. Should the regression line be added to the plot? Regression coefficients are calculated automatically.
#' @param rug Logical. Add marginal rug to the plot.
#' @param label_text A character vector of length=5 defining the names for the values in the info box.
#' @return a scatterplot of \code{x} and \code{y}.
#' @examples
#' x <- iris$Sepal.Length
#' y <- predict(lm(data=iris,iris$Sepal.Length~iris$Petal.Width))
#' scatter(x,y)
#' @references \url{https://github.com/ptompalski/scatter}
#' @export
#'
scatter <- function (x, y, R2=T, axisorder = "OP", xlab = "Observed",
                     ylab = "Predicted", title = NULL, info = T, position = 0,
                     positionauto = T, lowerlimit = NA, upperlimit = NA, alpha = 1, normality=T,
                     add.reg.line = F, rug = F, label_text = c("n", "bias", "bias%",
                                                               "RMSE", "RMSE%","p-value")){
  scatter::scatter(x,
                   y,
                   R2,
                   axisorder,
                   xlab,
                   ylab,
                   title,
                   info,
                   position,
                   positionauto,
                   lowerlimit,
                   upperlimit,
                   alpha,
                   normality,
                   add.reg.line,
                   rug,
                   label_text)
}
