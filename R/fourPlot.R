#' @title Four Plot
#'
#' @description The 4-plot is an EDA technique for testing underlying assumptions.
#'
#' @param data A list of data values.
#' @param bins Number of bins to show in the histogram.
#'
#' @return A frame with 4 plots.
#'
#' @details The 4-plot outputs a frame with four plots.
#' A run sequence plot to answer the questions of whether or not the data has fixed location and/or fixed variation.
#' A lag plot that show you whether or not the data is random.
#' A histogram to test if the data follows a normal distribution.
#' A normal probability plot that tests together with the histogram, if the data follows a normal distribution.
#' All plots are made with `ggplot2` and can be elaborated further if needed.
#'
#' `seq_plot` uses geom_line()
#'
#' `lag_plot` uses geom_point()
#'
#' `hist_plot` uses geom_histogram()
#'
#' `quant_plot` uses geom_qq()
#'
#' See the vignette for more details about this.
#'
#' @usage fourPlot(data, bins=11)
#'
#' @import ggplot2
#' @import tidyverse
#' @import ggpubr
#'
#' @examples
#' x <- LEW.DAT$Deflection
#' fourPlot(x)
#'
#' @export

fourPlot <- function(data, bins=11) {
  # Sequence plot made with geom_line
  seq_plot <- ggplot(mapping = aes(x = 1:length(data), y = data)) +
    labs(x = "RUN SEQUENCE PLOT Y", y = "") +
    geom_line()

  # lag plot made with geom_point
  lag_plot <- ggplot(mapping = aes(x=data, y=lag(data,1))) +
    labs(x = "LAG PLOT Y", y="") +
    geom_point(shape = 4, na.rm = TRUE)

  # Histogram made with geom_histogram
  hist_plot <- ggplot(mapping = aes(x=data)) +
    labs(x = "HISTOGRAM Y", y="") +
    geom_histogram(bins=bins)

  # Quantile plot made with geom_qq
  quant_plot <- ggplot(mapping = aes(sample=data)) +
    labs(x = "NORMAL PROBABILITY PLOT Y") +
    geom_qq(geom = "line")

  # Put all plots into a list
  fourplot_obj <- list(seq_plot = seq_plot,
                      lag_plot = lag_plot,
                      hist_plot = hist_plot,
                      quant_plot = quant_plot)

  # Assign class "4plot" to list
  class(fourplot_obj) <- "4plot"

  fourplot_obj
}

# Print function for class "4plot"
#' @export
print.4plot <- function(x, ...){
  fig <- ggarrange(x$seq_plot,
            x$lag_plot,
            x$hist_plot,
            x$quant_plot, ncol=2,nrow=2)
  print(annotate_figure(fig, top = text_grob("4-PLOT",size=16)))
  invisible(x)
}
