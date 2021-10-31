#' @title 4Plot
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'
#'

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


