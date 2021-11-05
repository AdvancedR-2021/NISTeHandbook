#' @title 4-Plot
#'
#' The 4-plot is an EDA technique for testing underlying assumptions. The 4-plot consists of four plots.
#' A run sequence plot, which is a simple plot of x and y values.
#' A lag plot.
#' A histogram.
#' A normal probability plot.
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

library(ggplot2)
library(tidyverse)
library("ggpubr")

fourPlot <- function(data, bins=11) {

  seq_plot <- ggplot(mapping = aes(x = 1:length(data), y = data)) +
    labs(x = "RUN SEQUENCE PLOT Y", y = "") +
    geom_line()

  lag_plot <- ggplot(mapping = aes(x=data, y=lag(data,1))) +
    labs(x = "LAG PLOT Y", y="") +
    geom_point(shape = 4)

  hist_plot <- ggplot(mapping = aes(x=data)) +
    labs(x = "HISTOGRAM Y", y="") +
    geom_histogram()

  quant_plot <- ggplot(mapping = aes(sample=data)) +
    labs(x = "NORMAL PROBABILITY PLOT Y") +
    geom_qq(geom = "line")

  fig <- ggarrange(seq_plot,lag_plot,hist_plot,quant_plot, ncol=2,nrow=2)
  annotate_figure(fig, top = text_grob("4-PLOT",size=16))

}


