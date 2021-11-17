#' Function to test for k outliers in both tails.
#'
#' @param x univariate data
#' @param k number of suspected outliers
#'
#' @return An object that shows results of hypothesis
#' @export
#'
#' @examples
#' x = c(-1.40, -0.44, -0.30, -0.24, -0.22, -0.13, -0.05,
#' 0.06, 0.10, 0.18, 0.20, 0.39, 0.48, 0.63, 1.01)
#' Specify k, the number of outliers being tested.
#' k = 3
#' TMTest(x,k)
#'
#' @export TMTest
TMTest <- function(x,k){
  ## Call the function and compute value of test statistic for data.
  ekstat = tm(x,k)
  ## Compute critical value based on simulation.
  test = c(1:100)
  for (i in 1:100){
    xx = rnorm(length(x))
    test[i] = tm(xx,k)
  }

  print(ggplot(mapping = aes(sample=x)) +
          geom_qq())

  lst <- list(ek = ekstat,
              h0 = "there are no outliers in the data",
              ha = c("the ",k, " most extreme points are outliers"),
              ek = ekstat,
              method = "Tietjen Moore Test",
              alpha = 0.05,
              low_tail =  quantile(test,0.05))
  class(lst) <- "tmtest"
  lst
}

#' @export
print.tmtest <- function(x, ...){
  cat("\nResults of ", x$method, "\n")
  cat("--------------------------\n")
  cat("H_0:  ", x$h0, "\n")
  cat("H_a:  ", x$ha, "\n\n")
  cat("Test statistic:  E_k = ", x$ek, "\n")
  cat("Significance level:  a = ", x$alpha, " \n")
  cat("Critical value for lower tail:  ", x$low_tail, " \n")
  cat("Critical region: Reject H_0 if E_k < ", x$low_tail, " \n")
  invisible(x)
}

# Helpers -----------------------------------------------------------------

tm <- function(x,k){

  n = length(x)

  ## Compute the absolute residuals.
  r = abs(x - mean(x))

  ## Sort data according to size of residual.
  df = data.frame(x,r)
  dfs = df[order(df$r),]

  ## Create a subset of the data without the largest k values.
  klarge = c((n-k+1):n)
  subx = dfs$x[-klarge]

  ## Compute the sums of squares.
  ksub = (subx - mean(subx))**2
  all = (df$x - mean(df$x))**2

  ## Compute the test statistic.
  ek = sum(ksub)/sum(all)
  ek
}
