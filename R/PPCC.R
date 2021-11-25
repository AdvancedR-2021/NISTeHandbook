#' @title Tukey-Lambda PPCC Plot
#'
#' @description The [Tukey-Lambda PPCC plot](https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm) is used to suggest an appropriate distribution for your data.
#'
#' @param data A list of data.
#'
#' @return The function returns a Tukey Lambda PPCC plot, the maximum value of the correlation coefficient
#' together with the corresponding lambda value and an indication of what kind of distribution your data might fallow.
#'
#' @details This function is useful for symmetric distributions. It can indicate what type of distribution your data follows,
#' for example if the data is long-tailed or short-tailed, based on the lambda value, and it can further indicate several common distributions.
#' The Tukey-Lambda PPCC plot is used to suggest an appropriate distribution and you should follow up with PPCC and
#' probability plots of the appropriate alternatives.
#'
#' @usage tukeyPPCC(data)
#'
#' @import ggplot2
#' @importFrom stats cor
#'
#' @examples
#' data <- rnorm(100)
#' tukeyPPCC(data)
#'
#' @export

tukeyPPCC <- function(data) {

  tukey_order <- function(mi, lambda){
    (mi**lambda - (1-mi)**lambda) / (lambda)
  }

  data <- sort(data)
  xval <- c()
  yval <- c()
  for (i in seq(-2,2,0.02)) {
    yval <- c(yval, stats::cor(tukey_order(U_i(data),i), data))
    xval <- c(xval, i)
  }

  max_corcoeff <- round(max(yval, na.rm=TRUE),digits=3)
  lambda <- round(xval[which.max(yval)],digits=3)

  if (lambda <= -0.70) {
    pred_dist <- "very long-tailed, close to -1 is approximately Cauchy"
  } else if (lambda == 0) {
    pred_dist <- "exactly logistic"
  } else if (lambda > -0.70 & lambda < 0.10) {
    pred_dist <- "long-tailed, for example double exponential or logistic"
  } else if (lambda >= 0.10 & lambda <= 0.18) {
    pred_dist <- "aprroximately normal"
  } else if (lambda == 1) {
    pred_dist <- "exactly uniform"
  } else if (lambda > 0.18) {
    pred_dist <- "short-tailed (U-shaped), for example beta or uniform"
  }

  cat(paste("Maximum value of the correlation coefficient", max_corcoeff, sep=" = "),
      paste(paste("Lambda", base::strrep(" ",37)),lambda, sep=" = "), sep = "\n")

  cat("The distribution of your data might be", pred_dist, sep=" ")

  ggplot2::ggplot(mapping = ggplot2::aes(x=xval, y=yval)) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x="Tukey Distribution Tail Length (Lambda)", y="Probability Plot Correlation Coefficient", title="(Tukey) PPCC Plot") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

#' @title PPCC Plot
#'
#' @description The [PPCC plot](https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm) is a graphical technique for identifying the shape parameter
#' for a distributional family that best describes the data set.
#'
#' @param data A list of data.
#' @param distribution The distribution that best describes your data.
#'
#' @return A Probability Plot Correlation Coefficient Plot.
#'
#' @details Firstly, you should your data through the `tukey_PPCC`-function to find an appropriate distribution
#' for your data. Then the PPCC plot is used first to find a good value of the shape parameter, and lastly
#' the probability plot (for example, use the function `ProbPlot`) should then be generated to find estimates of the location and scale parameters
#' and in addition to provide a graphical assessment of the adequacy of the distributional fit.
#'
#' You can only use data from either a weibull or gamma distribution, so far.
#'
#' @usage PPCC(data, distribution)
#'
#' @import ggplot2
#' @importFrom stats cor qweibull qgamma
#'
#' @examples
#' data <- RANDWEIB.DAT$Y
#' PPCC(data, "weibull")
#'
#' @export

PPCC <- function(data, distribution) {
  data <- sort(data)
  xval <- c()
  yval <- c()

  for (i in seq(0.1,10,0.01)) {
    if (distribution == "weibull") {
      yval <- c(yval, stats::cor(stats::qweibull(p=U_i(data),shape=i), data))
    }
    if (distribution == "gamma") {
      yval <- c(yval, stats::cor(stats::qgamma(p=U_i(data),shape=i), data))
    }
    xval <- c(xval, i)
  }
  cat(paste("Maximum value of the correlation coefficient", round(max(yval, na.rm=TRUE), digits=3), sep=" = "),
      paste(paste("Optimal value of the shape parameter", base::strrep(" ",7)), round(xval[which.max(yval)], digits=3), sep=" = "), sep="\n")

  ggplot2::ggplot(mapping = ggplot2::aes(x=xval, y=yval)) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x="Possible shape values (Lambda)", y="Probability Plot Correlation Coefficient", title=paste(distribution, "PPCC Plot")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


# Helpers ----------------------------------

U_i <- function(ordered_data){
  n <- length(ordered_data)
  m_i <- rep(NA, n)
  m_i[n] <- 0.5**(1/n)
  m_i[1] <- 1 - m_i[n]
  for (i in 2:n-1){
    m_i[i] <- (i - 0.3175)/(n + 0.365)
  }
  m_i
}
