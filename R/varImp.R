#' Returns variable importance
#'
#' When RF is used to find nearest neighbors, the importance of each variable in the RF trees is calculated. This function returns the importance of each variable and a \code{ggplot2} object
#'
#' If \code{scaled = TRUE}, importance values are centered by subtracting their mean and scaled by dividing the centered importance by their standard deviation.
#'
#' @param model A yai object
#' @param scaled Logical. Should importance values be centered and scaled?
#' @param plot Logical. If TRUE, returns a ggplot2 object based on plotType value
#' @param plotType Either of "boxplot" or "grid"
#' @return A list containing the following objects:
#'    \describe{
#'        \item{\code{importance}}{A data.frame object containing the importance of each response variable and the mean importance of all variables combined}
#'        \item{\code{plot}}{A ggplot object showing a plot of the importance values according to plotType}
#'    }
#' @seealso \code{\link[randomForest]{importance}}, \code{\link[yaImpute]{yaiVarImp}}
#'
#' @examples
#' # Load data
#' # kNN_model: trained kNN model (from trainNN)
#' load(system.file("extdata/examples/example_predictTrgs.RData", package = "foster"))
#'
#' varImp(kNN_model,scaled=FALSE,plot=TRUE,plotType="boxplot")
#' @export

varImp <- function(model,
                   scaled = TRUE,
                   plot = TRUE,
                   plotType = "boxplot") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("'ggplot2' package is needed for this function to work.",
         call. = FALSE)
  }

  # Define variables as NULL (fix "no visible binding for global variable" note during check)
  variable <- value <- var <-  NULL

  imp <- list()
  i <- 0
  for (Rf in model$ranForest) {
    i <- i + 1
    if (Rf$type == "regression") {
      attr <- "%IncMSE"
    } else {
      attr <- "MeanDecreaseAccuracy"
    }
    imp_var <- randomForest::importance(Rf)[, attr]

    if (scaled) {
      imp_var <- scale(imp_var, center = TRUE, scale = TRUE)
    }
    imp[[i]] <- imp_var
  }
  names(imp) <- attr(model$ranForest, "names")

  imp_combined <- eval(parse(text = paste0("data.frame(", paste0(names(imp),
                                  " = imp$", names(imp), collapse = ","), ")")))
  imp_combined$mean <- rowMeans(imp_combined[, names(imp)])
  imp_combined$variable <- rownames(imp_combined)
  imp_combined <- dplyr::arrange(imp_combined, mean)
  imp_combined$variable <- factor(imp_combined$variable, ordered = TRUE,
                                  levels = imp_combined$variable)

  if (plot) {
    imp_combined_long <- reshape2::melt(imp_combined, id.vars = "variable",
                            measure.vars = c(names(imp)), variable.name = "var")
    if (scaled) y_lab <- "Scaled importance" else y_lab <- attr

    if (plotType == "boxplot") {
      imp_combined_long <- reshape2::melt(imp_combined, id.vars = "variable",
                            measure.vars = c(names(imp)), variable.name = "var")

      p <- ggplot2::ggplot(imp_combined_long, ggplot2::aes(x = variable, y = value)) +
        ggplot2::geom_boxplot() +
        ggplot2::coord_flip() +
        ggplot2::ylab(y_lab) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid = ggplot2::element_blank())

    } else if (plotType == "grid") {
      imp_combined_long <- reshape2::melt(imp_combined, id.vars = "variable",
                    measure.vars = c(names(imp), "mean"), variable.name = "var")
      imp_combined_long$value <- round(imp_combined_long$value, 1)

      mid_val <- stats::median(imp_combined_long$value)

      p <- ggplot2::ggplot(imp_combined_long, ggplot2::aes(x = variable, y = var)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value)) +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_gradient2(
          low = "#4C7AB5", high = "#D93A2B", mid = "#F9FABE", name = y_lab,
          midpoint = mid_val,
          guide = ggplot2::guide_colourbar(title.position = "bottom", title.hjust = 0.5)
        ) +
        ggplot2::coord_equal(ratio = 1) +
        ggplot2::geom_text(ggplot2::aes(label = value)) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, size = 12, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 12),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(size = 12),
          legend.key.width = ggplot2::unit(3, "cm")
        )
    } else {
      warning("plotType not supported")
      p <- NULL
    }
  }
  return(list(
    importance = imp_combined,
    plot = p
  ))
}
