library(mosaic)
##Example 1: Consider two independent population of students (Bellevue College Students and ABCD College Students). We observed for the first group 143 students and for the second group 245 students.

#Find the test statistics
Z = (9.7-8.4)/sqrt((4/41)+(3/32))

#Distribution under the null hypothesis
plotDist('norm', mean=0, sd=1)
ladd( panel.abline(v=qnorm(alpha/2),col='red') )
ladd( panel.abline(v=qnorm(1-alpha/2),col='red') )
ladd( panel.abline(v=Z,col='purple',lwd=5) )

#Find the Critical region
alpha=0.05
xqnorm(alpha/2)
xqnorm(alpha/2,lower.tail = F)

#Find the Pvalue
1-xpnorm(Z)
