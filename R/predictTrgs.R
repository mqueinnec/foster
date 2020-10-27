#' Impute response variables across the landscape
#'
#' This function finds the k-NN of target observations and imputes response variables. \code{X} is a raster object where each layer correspond to one of the predictor variable used to train the k-NN model \code{model} obtained from \code{\link[foster]{trainNN}}.
#'
#'The method used to impute the NN is set from the kNN model trained by \code{\link[foster]{trainNN}}. If \code{k=1} the value of the single closest NN is imputed. If \code{k>1}, the closest, mean, median or weighted distance mean (default) of all k NN values is imputed. This is set using the \code{impute.cont} and \code{impute.fac} arguments of \code{\link[foster]{trainNN}}.
#'
#' The raster \code{x} is processed as blocks of \code{nrows} to avoid creating very large objects (several Gb) that couldn't be stored in memory. However, low values of \code{nrows} slow down processing. Depending on the amount of RAM available on your computer and on the size of the area where k-NN need to be calculated, it is possible to process more rows at the same time and considerably reduce processing time.
#'
#' @param model A trained kNN model obtained from \code{\link[foster]{trainNN}}
#' @param x Raster object where each layer corresponds to a predictor variable calculated at targets
#' @param nrows number of rows processed at a time. Default is 200 .
#' @param nnID Logical. Should the ID of each target's nearest neighbor used for imputation be returned?
#' @param nnDist Logical. Should the distance to each target's nearest neighbor used for imputation be returned?
#' @param filename Character. Output file name including path to directory and
#'   eventually extension.Default is \code{""} (output not written to disk).
#' @param par Logical. Should imputation be performed on parallel threads?
#' @param threads Integer. Number of parallel threads (relevant only if par=TRUE)
#' @param progress Logical. If TRUE (default) a progress bar is displayed.
#' @param ... Other arguments passed to \code{\link[raster]{writeRaster}}
#' @return A RasterStack object where the first layers correspond to the imputed response variables and the remaining layers to the nearest neighbor(s) ID (if \code{nnID = TRUE}) and nearest neighbor(s) distance (if \code{nnDist = TRUE})
#' @seealso \code{\link[yaImpute]{newtargets}}, \code{\link[yaImpute]{impute.yai}}
#'
#' @examples
#' # Load data
#' # kNN_model: trained kNN model (from trainNN)
#' # X_vars: RasterStack of predictor variables
#' load(system.file("extdata/examples/example_predictTrgs.RData", package =
#' "foster"))
#'
#' Y_imputed <- predictTrgs(model=kNN_model, x = X_vars, nnID = TRUE,
#' nnDist = TRUE)
#' @export

predictTrgs <- function(model = NULL,
                        x = NULL,
                        nrows = 200,
                        nnID = TRUE,
                        nnDist = TRUE,
                        filename = "",
                        par = FALSE,
                        threads = 2,
                        progress = TRUE,
                        ...) {

  # Check args
  if (threads <= 0) stop("threads must be > 0")
  if (is.null(model)) stop("Need to provide trained model")
  if (is.null(x)) stop("Need to provide targets X values")
  if (!all(colnames(model$xRefs) %in% names(x))) stop("Predictor variables
                                                      names don't match")

  if (is.null(model$impute.cont) | is.null(model$impute.fac)) {
    stop("Cannot find the imputation method (closest, mean, median or distWeighted) from the trained kNN model. Make sure that the model was created with foster::trainNN")
  }

  if (progress) {
    progress <- "text"
    print_pb <- TRUE
  }else{
    print_pb <- FALSE
    progress <- NULL
  }

  # If empty filename write to temp
  if (filename == "") {
    filename <- raster::rasterTmpFile()
  }

  # Create empty raster where predictions will be saved
  nlyr <- length(colnames(model$yRefs))
  if (nnID) {
    nlyr <- nlyr + model$k
  }
  if (nnDist) {
    nlyr <- nlyr + model$k
  }

  predrast <- raster::brick(x, values = FALSE, nl = nlyr)

  # Divide x in blocks.
  tr <- raster::blockSize(x, minblocks = round(nrow(x) / nrows))

  # Create progress bar
  pb <- raster::pbCreate(tr$n, label = "predict", progress = progress)

  if (!par | threads == 1) {
    predrast <- raster::writeStart(predrast, filename = filename, ...)

    for (i in 1:tr$n) {

      blockVals <- data.frame(raster::getValues(x, row = tr$row[i], nrows = tr$nrows[i]))

      predv <- predFun.yai(model, blockVals, nnID, nnDist)

      # Write preds to file
      predrast <- raster::writeValues(predrast, predv, tr$row[i])

      if(print_pb) raster::pbStep(pb, i)
    }
  } else if (par & threads > 1) {
    .sendCall <- eval(parse(text = "parallel:::sendCall"))
    .recvOneData <- eval(parse(text="parallel:::recvOneData"))

    # Start clusters
    raster::beginCluster(threads)

    cl <- raster::getCluster()
    on.exit(raster::endCluster())
    nodes <- length(cl)

    # Define function that will be executed by clusters
    clusfun <- function(fun, i) {

      blockVals <- data.frame(raster::getValues(x, row = tr$row[i], nrows = tr$nrows[i]))

      predv <- fun(model = model, data = blockVals, nnID = nnID, nnDist = nnDist)

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
      if(print_pb) raster::pbStep(pb, i)

      d <- .recvOneData(cl)

      if (!d$value$success) stop("cluster error")

      if (i == 1) {
        predrast <- raster::writeStart(predrast, filename = filename, ...)
      }

      predrast <- raster::writeValues(predrast, d$value$value, tr$row[d$value$tag])
      ni <- nodes + i

      if (ni <= tr$n) {
        .sendCall(cl[[d$node]], clusfun, list(fun = predFun.yai, ni), tag = ni)
      }
    }
  }

  predrast <- raster::writeStop(predrast)
  raster::pbClose(pb)
  raster::endCluster()

  # Set names of returned Raster
  if (nnID & !nnDist) {
    try(names(predrast) <- c(colnames(model$yRefs), paste0("nnID",
                                    as.character(seq(1:model$k)))), silent = TRUE)
  } else if (!nnID & nnDist) {
    try(names(predrast) <- c(colnames(model$yRefs), paste0("nnDist",
                                                           as.character(seq(1:model$k)))), silent = TRUE)
  } else if (nnDist & nnDist) {
    try(names(predrast) <- c(colnames(model$yRefs),
                             paste0("nnID", as.character(seq(1:model$k))),
                             paste0("nnDist", as.character(seq(1:model$k)))), silent = TRUE)
  } else {
    try(names(predrast) <- c(colnames(model$yRefs)), silent = TRUE)
  }

  return(predrast)
}



#' Predict function for yai
#'
#' Used in predictTrgs
#' @noRd

predFun.yai <- function(model, data, nnID, nnDist) {
  # Change rownames to avoid false duplicates
  rownames(data) <- paste0("trgs_", rownames(data))

  nrow_preds <- NROW(data)
  n_yvars <- ncol(model$yRefs)
  n_nnID <- ifelse(nnID, model$k, 0)
  n_nnDist <- ifelse(nnID, model$k, 0)

  ncols_preds <- n_yvars + n_nnID + n_nnDist

  rows_with_NA <- !stats::complete.cases(data)

  #Create template for output
  preds_NA <- data.frame(matrix(NA, nrow = nrow_preds, ncol = ncols_preds))
  rownames(preds_NA) <- rownames(data)

  out_colnames <- colnames(model$yRefs)
  out_colnames <- if(nnID) {
    c(out_colnames, paste0("nnID",seq(1:model$k)))
  }else{
    out_colnames
  }
  out_colnames <- if(nnDist) {
    c(out_colnames, paste0("nnDist",seq(1:model$k)))
  }else{
    out_colnames
  }

  colnames(preds_NA) <- out_colnames

  if (all(rows_with_NA)) {
    preds <- preds_NA
  }else{
    data_dropNA <- data[!rows_with_NA,]

    newtrgs <- yaImpute::newtargets(model, data_dropNA)

    trgs.imputed <- yaImpute::impute(newtrgs,
                                     observed = FALSE,
                                     vars = colnames(newtrgs$yRefs),
                                     method = model$impute.cont,
                                     method.factor = model$impute.fac)
    if (nnID) {
      nnID_df <- data.frame(matrix(as.numeric(as.character(newtrgs$neiIdsTrgs)),
                                   ncol = newtrgs$k,
                                   dimnames = list(NULL, paste0("nnID", as.character(seq(1:newtrgs$k))))))
      trgs.imputed <- cbind(trgs.imputed, nnID_df)
    }
    if(nnDist) {
      nnDist_df <- data.frame(matrix(as.numeric(newtrgs$neiDstTrgs),
                                     ncol = newtrgs$k,
                                     dimnames = list(NULL, paste0("nnDist", as.character(seq(1:newtrgs$k))))))
      trgs.imputed <- cbind(trgs.imputed,nnDist_df)
    }

    colnames(preds_NA) <- colnames(trgs.imputed)

    preds <- rbind(trgs.imputed, preds_NA[rows_with_NA,])

    #Reorder preds by rowname
    preds <- preds[rownames(data), ]

  }
  #tranform to matrix
  preds <- as.matrix(preds, rownames.force = FALSE)
  return(preds)
}
