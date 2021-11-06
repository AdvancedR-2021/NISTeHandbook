% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fourPlot.R
\name{fourPlot}
\alias{fourPlot}
\title{fourPlot}
\usage{
fourPlot(data, bins = 11)
}
\arguments{
\item{data}{A list of data values}

\item{bins}{Number of bins to show in the histogram}
}
\value{
A frame with 4 plots
}
\description{
The 4-plot is an EDA technique for testing underlying assumptions. The 4-plot consists of four plots.
A run sequence plot, which is a simple plot of x and y values.
A lag plot.
A histogram.
A normal probability plot.
}
\examples{
x <- LEW.DAT$Deflection
fourPlot(x)

}