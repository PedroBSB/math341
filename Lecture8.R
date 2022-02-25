library(mosaic)

## Example: The machine that fills 500-gram coffee containers for a large food processor is
#monitored by the quality control department.
weights = c(501.4,498.0,498.6,499.2,495.2,501.4,509.5,494.9,498.6,497.6,
            505.5,505.1,499.8,502.4,497.0,504.3,499.7,497.9,496.5,498.9,
            504.9,503.2,503.0,502.6,496.8,498.2,500.1,497.9,502.2,503.2)


#Find the statistics
s2 = var(weights)
n = length(weights)

#Find the boundaries for the Chi-Square(29)
LB = qchisq(.01/2, 29)
UB = qchisq(1 - .01/2, 29)

#Plot (Optional)
plotDist("chisq", params=c(29), col=c("red"))
ladd( panel.abline(h=0,v=LB,col='gray50') )
ladd( panel.abline(h=0,v=UB,col='gray50') )

#Find the confidence interval for sigma^2
LB_sigma2 = (n-1)*s2/UB
UB_sigma2 = (n-1)*s2/LB

#Find the confidence interval for sigma
LB_sigma = sqrt(LB_sigma2)
UB_sigma = sqrt(UB_sigma2)

#Thus, we are 99% confident that the standard deviation in the weights of the coffee
#containers lies between 2.56 and 5.10 grams.

## Example:The readings from the 20 diabetic patients are given here: 
measure = c(203.1,184.5,206.8,211.0,218.3,174.2,193.2,201.9,199.9,194.3,
            199.4,193.6,194.6,187.2,197.8,184.3,196.1,196.4,197.5,187.9)

#Under Null hypothesis
sigma2_0 = 5^2
n = length(measure)
s2 = var(measure)
alpha = 0.05

#Find the Critical Region
CR = qchisq(1-alpha,n-1)

#Find the test statistics
TS = (n-1)*s2/sigma2_0

#Plot (Optional)
plotDist("chisq", params=c(n-1), groups =  x > CR, 
         type="h",add=FALSE,under=TRUE,xlim=c(0,80))
ladd( panel.abline(h=0,v=TS,col='red') )

#Pvalue method
pvalue = 1-pchisq(TS,n-1)
