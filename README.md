# NISTeHandbook
R Package to implement methods from the NIST/SEMATECH e-Handbook of Statistical Methods.

Given that the user of the package has data, the package will implement the use of following methods:

1. 4-plot 

EDA techniques for testing underlying assumptions

    - run sequence plot
    - lag plot
    - histogram 
    - normal probability plot.

https://www.itl.nist.gov/div898/handbook/eda/section2/eda23.htm

2. Probability Plot Correlation Coefficient Plot:

a graphical technique for identifying the shape parameter for a distributional family that best describes the data set. 

    - generate a PPCC plot 
  
https://www.itl.nist.gov/div898/handbook/eda/section3/ppccplot.htm


3. Tietjen-Moore Test for Outliers

The Tietjen-Moore test is used to detect multiple outliers in a univariate data set that follows an approximately normal distribution. 

    - a normal probability plot should be generated along with a htest object.
  
https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm


4. 6 plot 

Collection of 6 specific graphical techniques whose purpose is to assess the validity of a Y versus X fit

    - Scatter plot of the response and predicted values versus the independent variable
    - Scatter plot of the residuals versus the independent variable
    - Scatter plot of the residuals versus the predicted values
    - Lag plot of the residuals
    - Histogram of the residuals
    - Normal probability plot of the residuals.

https://www.itl.nist.gov/div898/handbook/eda/section3/6plot.htm

------------

Project manager: Laura Horsfall Folmer 

Documentation manager: Laura Horsfall Folmer 

Quality manager: Tony Lung Ca Hin
