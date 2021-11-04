#' @title 6Plot
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' X <- 1:100
#' Y <- rnorm(x * 10, 0, 50)
#' sixPlot(X, Y)

library(ggplot2)
library(tidyverse)
library(ggpubr)

sixPlot <- function(X, Y) {
  linfit <- lm(Y ~ X)
  pred_Y <- predict(linfit)
  resid <- residuals(linfit)

  scat_plot <- ggplot(mapping = aes(x = X, y = pred_Y)) +
    geom_point() +
    labs(x = "PLOT Y VS X", y="")

  res_x_plot <- ggplot(mapping = aes(x = X, y = resid)) +
    geom_point() +
    labs(x = "PLOT RES X", y="")

  res_pred_plot <- ggplot(mapping = aes(x = pred_Y, y = resid)) +
    geom_point() +
    labs(x = "PLOT RES PRED", y="")

  lag_res_plot <- ggplot(mapping = aes(x = resid, y = lag(resid))) +
    geom_point() +
    labs(x = "LAG PLOT RES", y="")

  hist_res_plot <- ggplot(mapping = aes(x = resid)) +
    geom_histogram() +
    labs(x = "HISTOGRAM RES", y="")

  qq_plot <- ggplot(mapping = aes(sample=Y)) +
    labs(x = "NORM PROB PLOT RES") +
    geom_qq()

  fig <- ggarrange(scat_plot,
                   res_x_plot,
                   res_pred_plot,
                   lag_res_plot,
                   hist_res_plot,
                   qq_plot,
                   ncol=3,nrow=2)
  annotate_figure(fig, top = text_grob("6-PLOT",size=16))
}

X <- 1:100
Y <- rnorm(100, X * 2, 50)

sixPlot(X, Y)

