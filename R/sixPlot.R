
#' sixPlot
#'
#' The 6-plot is a collection of 6 specific graphical techniques
#' whose purpose is to assess the validity of a Y versus X fit.
#' The 6-plot consists of six plots.
#' Scatter plot of the response vs independent variables;
#'
#'
#' @param X A list of X values
#' @param Y A list of Y values
#' @param bins Number of bins to show in the histogram
#'
#' @return A frame with 6 plots
#'
#' @import ggplot2
#' @import stats
#' @import ggpubr
#'
#' @examples
#' X <- 1:100
#' Y <- rnorm(100, x * 10, 50)
#' sixPlot(X, Y)
#'
#' @export

sixPlot <- function(X, Y, bins=30) {
  linfit <- lm(Y ~ X)
  pred_Y <- predict(linfit)
  resid <- residuals(linfit)

  scat_plot <- ggplot2::ggplot(mapping = aes(x = X, y = pred_Y)) +
    geom_point() +
    labs(x = "PLOT Y PRED VS X", y="")

  res_x_plot <- ggplot(mapping = aes(x = X, y = resid)) +
    geom_point() +
    labs(x = "PLOT RES X", y="")

  res_pred_plot <- ggplot(mapping = aes(x = pred_Y, y = resid)) +
    geom_point() +
    labs(x = "PLOT RES PRED", y="")

  lag_res_plot <- ggplot(mapping = aes(x = resid, y = lag(resid))) +
    geom_point(na.rm = TRUE) +
    labs(x = "LAG PLOT RES", y="")

  hist_res_plot <- ggplot(mapping = aes(x = resid)) +
    geom_histogram(bins=bins) +
    labs(x = "HISTOGRAM RES", y="")

  qq_plot <- ggplot(mapping = aes(sample=Y)) +
    labs(x = "NORM PROB PLOT RES") +
    geom_qq()

  sixplot_obj <- list(scat_plot = scat_plot,
                       res_x_plot = res_x_plot,
                       res_pred_plot = res_pred_plot,
                       lag_res_plot = lag_res_plot,
                       hist_res_plot = hist_res_plot,
                       qq_plot = qq_plot)
  class(sixplot_obj) <- "6plot"
  print(sixplot_obj)
  sixplot_obj
}

<<<<<<< HEAD
# Helpers -------------------------------

=======

#' @export
print.6plot <- function(x, ...){
  fig <- ggarrange(x$scat_plot,
                   x$res_x_plot,
                   x$res_pred_plot,
                   x$lag_res_plot,
                   x$hist_res_plot,
                   x$qq_plot,ncol=3,nrow=2)
  print(annotate_figure(fig, top = text_grob("6-PLOT",size=16)))
  invisible(x)
}
>>>>>>> cee27b6375d62ef9675d349209c65bfb600b4939
