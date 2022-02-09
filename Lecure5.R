##### Example: Power of the test
library(mosaic)
#There were 13 girls among the 14 babies born to couples using this method. Let’s use a significance level of $\apha = 0.05$. 
#In addition to all of the given test components, finding power requires that we select a particular value of $p$ that is an alternative to the value assumed in the null hypothesis $H_{0}: p = 0.5$.
#Find the values of power corresponding to these alternative values of $p: 0.6, 0.7, 0.8, and 0.9$.

#Step 1: Find the critical value c such that P(X≥c |n=14,p=.5) is maximized, but still below 0.05.
c=qbinom(.95, 14, .5)

#Step 2 (Optional): Plot the rejection region under null hypothesis
plotDist("binom", params=c(14, .5), col=c("red","forestgreen"), groups=x >= 10)

#Step 2: Compute the Type 2 error against alternative value p=0.6 (This is the True State of Nature) is given by:
beta =  pbinom(c, 14, .6)

#Step 3 (Optional): Create the power curve
p_alternative = seq(.50, .9, by=.01)
p_type2_error = 1 - pbinom(10, 14, p_alternative)
plot(p_alternative, p_type2_error, type="l", main="Power Curve")


