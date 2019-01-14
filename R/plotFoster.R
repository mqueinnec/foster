


theme_pt <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + #%+replace%
    ggplot2::theme(
      #panel.border = ggplot2::element_rect(colour = "black", fill = F, size = 1),
      axis.text = ggplot2::element_text(margin = ggplot2::margin(10,10,10,10)),
      plot.margin = grid::unit(c(1.2, 1.2, 1.2, 1.2), "lines"),
      axis.ticks.length= grid::unit(0.15,"cm"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = F, size = 1),
      legend.key = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(10,0,0,0)),#element_text(hjust=0.5,vjust=0.5),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,10,0,0),angle=90),#(hjust=0.5,vjust=1.5,angle=90),
      #strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(lineheight=1.5),
      strip.background = ggplot2::element_blank(),#no border for facet titles
      legend.position="bottom", # legend on bottom
      legend.title = ggplot2::element_blank () #no title for legend
    )
}
bw <- theme_pt()

#' scatterplot with optional information on the errors between x and y.
#'
#' @param x a vector of observed data.
#' @param y a vector of predicted data.
#' @param by optional grouping variable. Can be character or factor
#' @param axisorder Optional. Set to \code{PO} (predicted-observed) to plot predicted (\code{y}) on the y-axis (this is the default). Set to \code{OP} (observed-predicted) to plot observed (\code{x}) on the y-axis.
#' @param xlab Optional. Title of the x-axis
#' @param ylab Optional. Title of the y-axis
#' @param info A logical value indictating whether information on count, bias and RMSE should be added to the plot.
#' @param position Determines the position of the info box
#' @param positionauto A logical value indicating whether the position of the info box should be optimized automaticaly.
#' @param lowerlimit A value determining the lower limit of the x and y axis
#' @param upperlimit A value determining the upper limit of the x and y axis
#' @param alpha Define the transparency of the points. 0 - fully transparent, 1 - opaque.
#' @param add.reg.line Logical. Should the regression line be added to the plot? Regression coeficients are calculated automatically.
#' @param add.reg.eq Logical. Should the regression equation be added to the plot?
#' @param rug Logical. Add marginal rug to the plot.
#' @param label A character vector containing the name of the metrics that are annotated on the plot. Must be in \code{c("count","R2","bias","bias%","RMSE","RMSE%","slope","intercept")}
#' @param label_text A character vector containing of the same length as label contaning th name of each label. Default is \code{c("n","R2","bias","bias%","RMSE","RMSE%","slope","intercept")}
#' @return a scatterplot of \code{x} and \code{y}.
#' @description This scatterplot is a wrapper function for a ggplot-based plot. It containes additional text panel that shows values calculated with \code{\link{calc.error}}
#' @examples
#' x <- iris$Sepal.Length
#' y <- predict(lm(data=iris,iris$Sepal.Length~iris$Petal.Width))
#' scatter(x,y)
#' @export

scatter <- function(predicted,observed,by=NULL,axisorder="OP",
                    xlab="Predicted",ylab="Observed",
                    title=NULL,info=T,
                    position=0,positionauto=T,
                    lowerlimit=NA,upperlimit=NA,
                    alpha=1,add.reg.line=F ,add.reg.eq=F,rug=F,
                    label = c("count","R2","bias","bias_per","RMSE","RMSE_per","slope","intercept"),
                    label_text = c("n","R2","bias","bias%","RMSE","RMSE%","slope","intercept")) {
  if (!is.null(by)) {
    data <- data.frame(x=predicted,y=observed,by=by)
    pts <- ggplot2::geom_point(shape=1,size=2,alpha=alpha,ggplot2::aes(colour=by))
  } else {
    data <- data.frame(x=predicted,y=observed)
    pts <- ggplot2::geom_point(shape=1,size=2,alpha=alpha)
  }

  if(axisorder == "PO") {
    data <- data.frame(x=observed,y=predicted)
    x_lab_copy <- xlab
    xlab=ylab
    ylab=x_lab_copy
  }

  all.labels <- c("count","R2","bias","bias_per","RMSE","RMSE_per","slope","intercept")
  all.labels.text <- c("n","R2","bias","bias%","RMSE","RMSE%","slope","intercept")

  if (any(!label %in% all.labels)){
    stop("Label not supported")
  }

  if(length(label_text)!=length(label)){ stop("label and label_text must have the same length")}

  d <- foster::calc_error(reference = observed,estimate = predicted)


  eval.label <- character()


  lm.model <- lm(y ~ x,data=data)
  lm.coefs <- coefficients(lm.model)


  for (l in 1:length(label)){
    if (label[l] == 'slope'){
      eval.label[l] <- paste0(label_text[l],"=",round(lm.coefs[2],2))
    }else if (label[l] == 'intercept'){
      eval.label[l] <- paste0(label_text[l],"=",round(lm.coefs[1],2))
    }else{
      eval.label[l] <- paste0(label_text[l],"=",round(d[[label[l]]],2))
    }
    if(l>1){
      eval.label[l] <- paste0("\n",eval.label[l])
    }
  }
  eval.label <- paste0(eval.label,collapse = "")

  # label <- paste(label_text[1]," = ",d$count,
  #                "\n",label_text[2]," = ",round(d$R2,2),
  #                "\n",label_text[3]," = ",round(d$bias,3),
  #                "\n",label_text[4]," = ",round(d$bias_per,2)," %",
  #                "\n",label_text[5]," = ",round(d$RMSE,3),
  #                "\n",label_text[6]," = ",round(d$RMSE_per,2)," %",
  #                "\n",label_text[7]," = ",round(coefficients(lm.model)[2],2),
  #                "\n",label_text[8]," = ",round(coefficients(lm.model)[1],2),
  #                sep="")


  if (is.na(lowerlimit))  lowerlimit <- min(data[c("x","y")],na.rm=T)
  if (is.na(upperlimit))  upperlimit <- max(data[c("x","y")],na.rm=T)

  if(position != 0) positionauto <- F
  if(positionauto == T) {
    if (is.finite(d$bias_per) & d$bias_per < -20) position <- 1
  }

  if(position == 0)  {ann_x <- upperlimit; ann_y <- -Inf; ann_hjust <- 1; ann_vjust <- -0.2}
  if(position == 1)  {ann_x <- lowerlimit; ann_y <- upperlimit; ann_hjust <- 0; ann_vjust <- 0.9}

  if (info==T) {ann <- ggplot2::annotate("text",x=ann_x,y=ann_y,label=eval.label,hjust=ann_hjust,vjust=ann_vjust)} else {ann<-bw}
  if (rug==T) {addrug <- ggplot2::geom_rug(alpha=0.2)} else {addrug <-bw}

  if (add.reg.line==T) {reg.line <- ggplot2::geom_smooth(se = FALSE,method="lm",colour="red")} else {reg.line <- bw}

  plot <- ggplot2::ggplot(data=data,ggplot2::aes(x=x,y=y))  +
    pts +
    ggplot2::xlab(xlab)+
    ggplot2::ylab(ylab)+
    ggplot2::ggtitle(title) +
    ggplot2::xlim(lowerlimit,upperlimit) +
    ggplot2::ylim(lowerlimit,upperlimit) +
    ggplot2::geom_abline(intercept=0,slope=1,linetype='dashed')+
    ann+
    ggplot2::theme(legend.position='bottom')+
    ggplot2::coord_equal(ratio =1) +
    addrug +
    reg.line +
    bw

  return(plot)
}
