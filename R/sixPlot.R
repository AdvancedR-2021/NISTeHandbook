#' @title Six Plot
#'
#' @description The [6-plot](https://www.itl.nist.gov/div898/handbook/eda/section3/6plot.htm) is a collection of 6 specific graphical techniques
#' whose purpose is to assess the validity of a Y versus X fit. It returns a list of 6 plots;
#'
#' - [Scatter plot (predicted vs independent variable)](https://www.itl.nist.gov/div898/handbook/eda/section3/scatterp.htm)
#' - [Scatter plot (residuals vs independent variable)](https://www.itl.nist.gov/div898/handbook/eda/section3/scatterp.htm)
#' - [Scatter plot (residuals vs predicted values)](https://www.itl.nist.gov/div898/handbook/eda/section3/scatterp.htm)
#' - [Lag plot of residuals](https://www.itl.nist.gov/div898/handbook/eda/section3/lagplot.htm)
#' - [Histogram of residuals](https://www.itl.nist.gov/div898/handbook/eda/section3/histogra.htm)
#' - [Normal probability plot of the residuals](https://www.itl.nist.gov/div898/handbook/eda/section3/normprpl.htm)
#'
#' @param X A list of X values.
#' @param Y A list of Y values.
#' @param bins Number of bins to show in the histogram.
#'
#' @return A frame with 6 plots.
#'
#' @details The 6-plot outputs a 6plot object with six plots. A method is provided to print them in a frame.
#' The plots are named scat_plot, res_x_plot, res_pred_plot,lag_res_plot, hist_res_plot and qq_plot.
#' The model is a linear fit.
#'
#' - `scat_plot` uses geom_point()
#'
#' - `res_x_plot` uses geom_point()
#'
#' - `res_pred_plot` uses geom_point()
#'
#' - `lag_res_plot` uses geom_point()
#'
#' - `hist_res_plot` uses geom_histogram()
#'
#' - `qq_plot` uses geom_qq()
#'
#' @usage sixPlot(X, Y, bins=30)
#'
#' @import ggplot2
#' @importFrom stats lm predict residuals
#' @import ggpubr
#' @importFrom dplyr lag
#'
#' @examples
#' X <- 1:100
#' Y <- rnorm(100, x * 10, 50)
#' sixPlot(X, Y)
#'
#' @export

sixPlot <- function(X, Y, bins=30) {
  assertthat::assert_that(is.numeric(X))
  assertthat::assert_that(is.numeric(Y))
  assertthat::assert_that(length(X) == length(Y), msg = "Lengths of X and Y vector are not equal")

  # Fit the best linear model on the data and save predicted values and residuals
  linfit <- stats::lm(Y ~ X)
  pred_Y <- stats::predict(linfit)
  resid <- stats::residuals(linfit)

  # Scatter plot of response vs predicted values made with geom_point
  scat_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = X, y = pred_Y)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "PLOT Y PRED VS X", y="")

  # Scatter plot of residuals vs independent values made with geom_point
  res_x_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = X, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "PLOT RES X", y="")

  # Scatter plot of residuals vs predicted values made with geom_point
  res_pred_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = pred_Y, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "PLOT RES PRED", y="")

  # Lag plot of residuals made with geom_point
  lag_res_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = resid, y = dplyr::lag(resid,1))) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x = "LAG PLOT RES", y="")

  # Histogram of residuals made with geom_histogram
  hist_res_plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(bins=bins) +
    ggplot2::labs(x = "HISTOGRAM RES", y="")

  # Quantile plot of residuals made with geom_qq
  qq_plot <- ggplot2::ggplot(mapping = ggplot2::aes(sample=Y)) +
    ggplot2::labs(x = "NORM PROB PLOT RES") +
    ggplot2::geom_qq()

  # Put all plots into a list
  sixplot_obj <- list(scat_plot = scat_plot,
                       res_x_plot = res_x_plot,
                       res_pred_plot = res_pred_plot,
                       lag_res_plot = lag_res_plot,
                       hist_res_plot = hist_res_plot,
                       qq_plot = qq_plot)

  # Assign class "6plot" to list
  class(sixplot_obj) <- "6plot"

  sixplot_obj
}

# Print function for class "6plot"
#' @export
print.6plot <- function(x, ...){
  fig <- ggpubr::ggarrange(x$scat_plot,
                   x$res_x_plot,
                   x$res_pred_plot,
                   x$lag_res_plot,
                   x$hist_res_plot,
                   x$qq_plot,ncol=3,nrow=2)
  print(ggpubr::annotate_figure(fig, top = ggpubr::text_grob("6-PLOT",size=16)))
  invisible(x)
}

