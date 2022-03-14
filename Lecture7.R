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


##Example 2: Consider two independent population of students (Bellevue College Students and ABCD College Students). We observed for the first group 143 students and for the second group 245 students.
fresh = c(10.2,10.6,10.5,10.7,10.3,10.2,10.8,10.0,9.8,10.6)
stored = c(9.8,9.7,9.6,9.5,10.1,9.6,10.2,9.8,10.1,9.9)
alpha = 0.025
#Compute the summary statistics (fresh)
n1 = length(fresh)
x_bar1 = mean(fresh)
s1.2 = var(fresh)


#Compute the summary statistics (stored)
n2 = length(stored)
x_bar2 = mean(stored)
s2.2 = var(stored)

#Compute the pooled standard deviation
sp = sqrt(((n1-1)*s1.2+(n2-1)*s2.2)/(n1+n2-2))

#Compute the Confidence Interval
LB=(x_bar1-x_bar2)-qt(1-alpha/2,df=n1+n2-2)*sp*sqrt(1/10+1/10)
UB=(x_bar1-x_bar2)+qt(1-alpha/2,df=n1+n2-2)*sp*sqrt(1/10+1/10)


##Example 4:  An experiment was conducted to evaluate the effectiveness of a treatment for tapeworm in the stomachs of sheep.
x2 = c(18,43,28,50,16,32,13,35,38,33,6,7)
x1 = c(40,54,26,63,21,37,39,23,48,58,28,39)

alpha = 0.05
#Compute the summary statistics (X1)
n1 = length(x1)
x_bar1 = mean(x1)
s1.2 = var(x1)


#Compute the summary statistics (X2)
n2 = length(x2)
x_bar2 = mean(x2)
s2.2 = var(x2)

#Pooled standard deviation
sp = sqrt(((n1-1)*s1.2+(n2-1)*s2.2)/(n1+n2-2))

#Find the Critical region: #Distribution under the null hypothesis
plotDist('t', df=n1+n2-2)
ladd( panel.abline(v=qt(1-alpha,df=n1+n2-2),col='red') )

#Find the test statistic
D0 = 5
T = ((x_bar1-x_bar2)-D0)/(sp*sqrt(1/n1+1/n2))
ladd( panel.abline(v=T,col='purple',lwd=5) )

#Paired data
garage_1 = c(17.6,20.2,19.5,11.3,13.0,16.3,15.3,16.2,12.2,14.8,21.3,22.1,16.9,17.6,18.4)
garage_2 = c(17.3,19.1,18.4,11.5,12.7,15.8,14.9,15.3,12.0,14.2,21.0,21.0,16.1,16.7,17.5)

#Descriptive Analysis
plot(garage_1,garage_2)
cor(garage_1,garage_2)
cov(garage_1,garage_2)

#More interesting plot
# Add regression line
plot(garage_1, garage_2, main = "Garage exercise",
     xlab = "Garage 1", ylab = "Garage 2",
     pch = 19, frame = FALSE)
#Add the linear regression
abline(lm(garage_2 ~ garage_1), col = "blue")
