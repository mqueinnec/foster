#'Calculate median, IQR and Theil Sen slope
#'
#'This function is usually called within \code{\link[foster]{temporalMetrics}}
#'
#'@param x Vector of numeric values
#'@return A list with median, IQR and slope
#'@export
defaultTemporalSummary <- function(x) {
  if (!requireNamespace("wql",quietly=TRUE)){
    stop("Package \"wql\" needed by defaultTemporalSummary to calculate Then-Sen slope. Please install it.")
  }
  c(
    mean=mean(x,na.rm=T),
    sd=sd(x,na.rm=T),
    median=median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}



