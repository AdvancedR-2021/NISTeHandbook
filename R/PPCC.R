# Help function
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

#' PPCC_tukey
#'
#' @param data A list of data values
#'
#' @return what it returns
#'
#' @export
#'
PPCC_tukey <- function(data) {

  tukey_order <- function(mi, lambda){
    (mi**lambda - (1-mi)**lambda) / (lambda)
  }

  data <- sort(data)
  xval <- c()
  yval <- c()
  for (i in seq(-2,2,0.02)) {
    yval <- c(yval, cor(tukey_order(U_i(data),i), data))
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
      paste(paste("Lambda", strrep(" ",37)),lambda, sep=" = "), sep = "\n")

  cat("The distribution of your data is", pred_dist, sep=" ")

  ggplot(mapping = aes(x=xval, y=yval)) +
    geom_point(na.rm = TRUE)

} # Tukey Lambda PPCC can indicate what type of distribution your data follows, for example if the data is long-tailed or short-tailed, based on the lambda value.


PPCC <- function(data, distribution) {
  data <- sort(data)
  xval <- c()
  yval <- c()

  for (i in seq(0.1,10,0.01)) {
    if (distribution == "weibull") {
      yval <- c(yval, cor(dweibull(x=U_i(data),shape=i), data))
    }
    if (distribution == "gamma") {
      yval <- c(yval, cor(dgamma(x=U_i(data),shape=i), data))
    }
    xval <- c(xval, i)
  }
  print(max(yval, na.rm=TRUE))
  print(xval[which.max(yval)])
  ggplot(mapping = aes(x=xval, y=yval)) +
    geom_point(na.rm = TRUE)
} # The maximum correlation coefficient corresponds to the optimal value of the shape parameter.




# The Tukey-Lambda PPCC plot is used to suggest an appropriate distribution.
# You should follow-up with PPCC and probability plots of the appropriate alternatives.
# https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm

# To make the probability plot we use:
# https://www.itl.nist.gov/div898/handbook/eda/section3/probplot.htm
# For the Tukey Lambda dist: https://www.itl.nist.gov/div898/handbook/eda/section3/eda366f.htm


