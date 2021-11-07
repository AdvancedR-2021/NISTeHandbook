library(tidyverse)


## Input data.
x = c(-1.40, -0.44, -0.30, -0.24, -0.22, -0.13, -0.05,
      0.06, 0.10, 0.18, 0.20, 0.39, 0.48, 0.63, 1.01)

## Specify k, the number of outliers being tested.
k = 3

## Generate normal probability plot.
qqnorm(x)

boxplot(x)

## Create a function to compute statistic to
## test for outliers in both tails.
TMTest = function(x,k){
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
              ha = cat("the",k, "most extreme points are outliers"),
              ek = ekstat,
              method = "Tietjen Moore Test",
              alpha = 0.05,
              low_tail = cat("Critical value for lower tail ", ekstat),
              region = cat("Critical region: Reject H0 if E_k < ", quantile(test,0.05))
              )
  class(lst) <- "tmtest"
  print(lst)

}

print.tmtest <- function(x, ...){
  print(x$h0)
  print(x$ha)
  print(x$ek)
  print(x$method)
  print(x$alpha)
  print(x$low_tail)
  print(x$region)
  invisible(x)
}

TMTest(x,2)

