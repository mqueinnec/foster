#' Return Y values at targets
#'
#' This function finds the k-NN of target observations and imputes response variables. \code{X} is a raster object where each layer correspond to one of the predictor variable used to train the k-NN model \code{model}.
#'
#' By default, the data is processed row by row to avoid creating very large objects (several Gb) that couldn't be stored in memory. However, processing data row by row slows down processing. Depending on the amount of RAM available on your computer and on the size of the area where k-NN need to be calculated, it is possible to process multiple rows at the same time and considerably reduce processing times.
#'
#' @param model \code{yai} object. A model trained at reference locations, usually created from \code{\link[foster]{findNN}}
#' @param x Raster object where each layer corresponds to a predictor variable calculated at targets
#' @param nrows number of rows processed at a time. Default is 1.
#' @param nnID Logical. Should the ID of each target's nearest neighbor used for imputation be returned?
#' @param filename Character (optional). Output filename including path to
#' directory and eventually extension
#' @param par Logical. Should imputation be performed on parallel threads?
#' @param threads Integer. Number of parallel threads (relevant only if par=TRUE)
#' @param progress Logical. If TRUE (default) a progress bar is displayed.
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#' @return A RasterStack object where the first layers correspond to the imputed response variables and the remaining layers to the nearest neighbor ID (if \code{nnID = TRUE})
#' @seealso \code{\link[yaImpute]{newtargets}}, \code{\link[yaImpute]{impute.yai}}
#' @export

predictTrgs <- function(model = NULL,
                        x = NULL,
                        nrows = 1,
                        nnID = TRUE,
                        filename = "",
                        par = F,
                        threads = 2,
                        progress = TRUE,
                        ...) {

  # Check args
  if (threads <= 0) stop("threads must be > 0")
  if (is.null(model)) stop("Need to provide trained model")
  if (is.null(x)) stop("Need to provide targets X values")
  if (!all(colnames(model$xRefs) %in% names(x))) stop("Predictor variables
                                                      names don't match")

  if (progress) progress <- "text" else progress <- NULL

  # Define predict function
  predFun.yai <- function(model, data, nnID) {
    newtrgs <- yaImpute::newtargets(model, data)
    trgs.imputed <- yaImpute::impute(newtrgs, observed = F,
                                     vars = colnames(newtrgs$yRefs))
    if (nnID) {
      nnID <- matrix(as.numeric(as.character(newtrgs$neiIdsTrgs)),
        ncol = newtrgs$k,
        dimnames = list(NULL, paste0("nnID", as.character(seq(1:newtrgs$k)))))
      preds <- cbind(as.matrix(trgs.imputed, rownames.force = F), nnID)
    } else {
      preds <- as.matrix(trgs.imputed, rownames.force = F)
    }

    return(preds)
  }

  # If empty filename write to temp
  if (filename == "") {
    filename <- rasterTmpFile()
  }

  # Create empty raster where predictions will be saved
  if (nnID) {
    nlyr <- length(colnames(model$yRefs)) + model$k
  } else {
    nlyr <- length(colnames(model$yRefs))
  }

  predrast <- brick(x, values = FALSE, nl = nlyr)

  # Divide x in blocks.
  tr <- raster::blockSize(x, minblocks = round(nrow(x) / nrows))

  # Create progress bar
  pb <- pbCreate(tr$n, label = "predict", progress = progress)

  if (!par | threads == 1) {
    predrast <- writeStart(predrast, filename = filename, ...)

    for (i in 1:tr$n) {
      napred <- matrix(rep(NA, ncol(predrast) * tr$nrows[i] * nlayers(predrast))
                       , ncol = nlayers(predrast))

      blockVals <- data.frame(getValues(x, row = tr$row[i], nrows = tr$nrows[i]))

      # Remove NAs from blockVals
      blockVals <- stats::na.omit(blockVals)

      if (nrow(blockVals) == 0) {
        predv <- napred
      } else {
        # Change rownames to avoid false duplicates with yai
        rownames(blockVals) <- paste0("trgs_", rownames(blockVals))
        # Predict
        predv <- predFun.yai(model, blockVals, nnID)
      }
      # Assign back NA
      naind <- as.vector(attr(blockVals, "na.action"))
      if (!is.null(naind)) {
        p <- napred
        p[-naind, ] <- predv
        predv <- p
        rm(p)
      }
      # Write preds to file
      predrast <- writeValues(predrast, predv, tr$row[i])

      raster::pbStep(pb, i)
    }
  } else if (par & threads > 1) {
    .sendCall <- eval(parse(text = "parallel:::sendCall"))

    # Start clusters
    beginCluster(threads)

    cl <- getCluster()
    on.exit(endCluster())
    nodes <- length(cl)

    # Define function that will be exectuted by clusters
    clusfun <- function(fun, i) {
      napred <- matrix(rep(NA, ncol(predrast) * tr$nrows[i] * nlayers(predrast))
                       , ncol = nlayers(predrast))
      blockVals <- data.frame(getValues(x, row = tr$row[i], nrows = tr$nrows[i]))
      # Remove NAs from blockVals
      blockVals <- stats::na.omit(blockVals)
      if (nrow(blockVals) == 0) {
        predv <- napred
      } else {
        # Change rownames to avoid false duplicates with yai
        rownames(blockVals) <- paste0("trgs_", rownames(blockVals))
        # Predict
        predv <- fun(model = model, data = blockVals, nnID = nnID)
      }
      # Assign back NA
      naind <- as.vector(attr(blockVals, "na.action"))
      if (!is.null(naind)) {
        p <- napred
        p[-naind, ] <- predv
        predv <- p
        rm(p)
      }
      return(predv)
    }

    #If there are more nodes than blocks we don't need all clusters
    if (tr$n < nodes) {
      nodes <- tr$n
    }

    # Send function to be executed in clusters
    for (i in 1:nodes) {
      .sendCall(cl[[i]], clusfun, list(fun = predFun.yai, i = i), tag = i)
    }

    for (i in 1:tr$n) {
      pbStep(pb, i)

      d <- snow::recvOneData(cl)

      if (!d$value$success) stop("cluster error")

      if (i == 1) {
        predrast <- writeStart(predrast, filename = filename, ...)
      }

      predrast <- writeValues(predrast, d$value$value, tr$row[d$value$tag])
      ni <- nodes + i

      if (ni <= tr$n) {
        .sendCall(cl[[d$node]], clusfun, list(fun = predFun.yai, ni), tag = ni)
      }
    }
  }
  if (nnID) {
    try(names(predrast) <- c(colnames(model$yRefs), paste0("nnID",
                                    as.character(seq(1:model$k)))), silent = T)
  } else {
    try(names(predrast) <- c(colnames(model$yRefs)), silent = T)
  }

  predrast <- writeStop(predrast)
  pbClose(pb)
  endCluster()
  return(predrast)
}
