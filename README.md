# NISTeHandbook
R Package to implement methods from the NIST/SEMATECH e-Handbook of Statistical Methods.

Given that the user of the package has data, the package will implement the use of following methods:

1. 4-plot 

EDA techniques for testing underlying assumptions

  1. run sequence plot
  2. lag plot
  3. histogram 
  4. normal probability plot.

https://www.itl.nist.gov/div898/handbook/eda/section2/eda23.htm

2. Probability Plot Correlation Coefficient Plot:

a graphical technique for identifying the shape parameter for a distributional family that best describes the data set. 

  1. generate a PPCC plot 
  
https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm


3. Tietjen-Moore Test for Outliers

The Tietjen-Moore test is used to detect multiple outliers in a univariate data set that follows an approximately normal distribution. 

  1. a normal probability plot should be generated along with a htest object.
  
https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm


4. 6 plot 

Collection of 6 specific graphical techniques whose purpose is to assess the validity of a Y versus X fit

  1. Scatter plot of the response and predicted values versus the independent variable
  2. Scatter plot of the residuals versus the independent variable
  3. Scatter plot of the residuals versus the predicted values
  4. Lag plot of the residuals
  5. Histogram of the residuals
  6. Normal probability plot of the residuals.

https://www.itl.nist.gov/div898/handbook/eda/section3/6plot.htm


Project manager: Laura Horsfall Folmer 

Documentation manager: Laura Horsfall Folmer 

Quality manager: Tony Lung Ca Hin
