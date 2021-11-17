library(ggplot2)
library(tidyverse)
library(ggpubr)
#' fourPlot
#'
#' The 4-plot is an EDA technique for testing underlying assumptions. The 4-plot consists of four plots.
#' A run sequence plot to answer the questions of whether or not the data has fixed location and/or fixed variation.
#' A lag plot that show you whether or not the data is random.
#' A histogram to test if the data follows a normal distribution.
#' A normal probability plot that tests together with the histogram, if the data follows a normal distribution.
#'
#' @param data A list of data values
#' @param bins Number of bins to show in the histogram
#'
#' @return A frame with 4 plots
#'
#' @examples
#' x <- LEW.DAT$Deflection
#' fourPlot(x)
#'
#' @export

fourPlot <- function(data, bins=11) {
  seq_plot <- ggplot(mapping = aes(x = 1:length(data), y = data)) +
    labs(x = "RUN SEQUENCE PLOT Y", y = "") +
    geom_line()

  lag_plot <- ggplot(mapping = aes(x=data, y=lag(data,1))) +
    labs(x = "LAG PLOT Y", y="") +
    geom_point(shape = 4, na.rm = TRUE)

  hist_plot <- ggplot(mapping = aes(x=data)) +
    labs(x = "HISTOGRAM Y", y="") +
    geom_histogram()

  quant_plot <- ggplot(mapping = aes(sample=data)) +
    labs(x = "NORMAL PROBABILITY PLOT Y") +
    geom_qq(geom = "line")

  fourplot_obj <- list(seq_plot = seq_plot,
                      lag_plot = lag_plot,
                      hist_plot = hist_plot,
                      quant_plot = quant_plot)
  class(fourplot_obj) <- "4plot"
  print(fourplot_obj)
  fourplot_obj
}

#' @export
print.4plot <- function(x, ...){
  fig <- ggarrange(x$seq_plot,
            x$lag_plot,
            x$hist_plot,
            x$quant_plot, ncol=2,nrow=2)
  print(annotate_figure(fig, top = text_grob("4-PLOT",size=16)))
  invisible(x)
}
