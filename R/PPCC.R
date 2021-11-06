#' PPCC
#'
#' @param data The data
#' @param distribution Description
#'
#' @return what it returns
#'
#' @export
#'
PPCC <- function(data, distribution) {

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

  tukey_order <- function(mi, lambda){
    (mi**lambda - (1-mi)**lambda) / lambda
  }

  data <- sort(data)
  xval <- c()
  yval <- c()
  for (i in seq(-2,2,0.1)) {
    yval <- c(yval, cor(tukey_order(U_i(data),i), data))
    xval <- c(xval, i)
  }
  print(max(yval, na.rm=TRUE))
  print(xval[which.max(yval)])
  plot(xval, yval)
}

# The Tukey-Lambda PPCC plot is used to suggest an appropriate distribution.
# You should follow-up with PPCC and probability plots of the appropriate alternatives.
# https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm

# To make the probability plot we use:
# https://www.itl.nist.gov/div898/handbook/eda/section3/probplot.htm
# For the Tukey Lambda dist: https://www.itl.nist.gov/div898/handbook/eda/section3/eda366f.htm




