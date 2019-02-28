#'Calculate correlation between variables and discard the most correlated ones
#'
#'This is a wrapper function of \code{\link[caret]{findCorrelation}}.
#'
#'@param x a numeric vector, matrix or dataframe
#'@param vars Numeric or character vector indicatig which variables (columns) should be considered. By default, all columns are used.
#'@param use an optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#'@param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}: can be abbreviated.
#'@param cutoff Numeric value for the pair-wise aboslute correlation cutoff
#'@param verbose logical. Should details be printed?
#'@param names logical. Should column names (TRUE) pr column index (FALSE) be returned?
#'@param exact logical. Should the average correlations be recomputed at each step? See details in documentation of \code{\link[caret]{findCorrelation}}
#'@return A list containing a new dataframe without most correlated variables and a vector with the columns index or names that have been removed
#'@export

varCor <- function(x,
                   vars=1:ncol(x),
                   use="everything",
                   method=c("pearson","kendall","spearman"),
                   cutoff=0.9,
                   verbose=FALSE,
                   names=FALSE,
                   exact=ncol(x) < 100){

  x <- x[,vars]
  cor.mat <- stats::cor(x,use=use,method=method)
  highCor <- caret::findCorrelation(cor.mat,cutoff=cutoff,verbose=verbose,names=names,exact=ncol(x)<100)

  if(!names){
    new.df <- x[,-highCor]
  }else{
    new.df <- x[,setdiff(colnames(x),highCor)]
  }

  out <- list(
    x = new.df,
    highCorVar = highCor
  )
  return(out)
}
