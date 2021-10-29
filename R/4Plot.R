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

fourPlot <- function(data) {

  data <- data

  seq_plot <- data %>%
    ggplot(mapping = aes(x = 1:length(Y), y = Y)) +
    labs(x = "Index", y = "Transmittance") +
    geom_point(shape = 4)
  #print(seq_plot)

  data$Ylag <- lag(data$Y,1)

  lag_plot <- data %>%
    ggplot(mapping = aes(x=Ylag, y=Y)) +
    labs(x = expression(Y[i-1]), y=expression(Y[i])) +
    geom_point(shape = 4)
  #print(lag_plot)

  hist_plot <- data %>%
    ggplot(mapping = aes(x=Y)) +
    geom_histogram()
  print(hist_plot)
}

