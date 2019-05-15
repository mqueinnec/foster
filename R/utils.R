#' Calculates median, IQR and Theil Sen slope
#'
#' This function is usually called within \code{\link[foster]{temporalMetrics}}
#'
#' @param x Vector of numeric values
#' @return A list with median, IQR and slope
#' @export
#'
defaultTemporalSummary <- function(x) {
  if (!requireNamespace("wql", quietly = TRUE)) {
    stop("Package \"wql\" needed for defaultTemporalSummary to work.
         Please install it.", call. = FALSE)
  }
  c(
    median = median(x, na.rm = T),
    IQR = IQR(x, na.rm = T),
    slope = as.numeric(wql::mannKen(x)[1])
  )
}

#' Convert a SPDF to data.frame
#'
#' Convert a \code{\link[sp]{SpatialPoints}} to data.frame. Coordinates of \code{x} can be added to the data.frame using \code{xy = T}
#'
#' @param x A \code{\link[sp]{SpatialPoints}} object
#' @param xy Logical. If TRUE, coordinates of \code{x} are added to data.frame
#' @export

spdf2df <- function(x, xy = F) {
  out <- raster::as.data.frame(x)
  if (!xy) {
    coord.name <- sp::coordnames(x)
    out[, coord.name[1]] <- NULL
    out[, coord.name[2]] <- NULL
  }
  return(out)
}
