#Call libraries
library(tidyr)
library(mosaic)

#ANOVA Computation
X1=c(7,3,6,6)
X2=c(6,5,5,8)
X3=c(4,7,6,7)
n=length(X1)
g=3
#Means
mean_x1 = mean(X1)
mean_x2 = mean(X2)
mean_x3 = mean(X3)

#Variances
var_x1 = var(X1)
var_x2 = var(X2)
var_x3 = var(X3)

#Compute the Variance Between Samples
between_variance = var(c(mean_x1,mean_x2,mean_x3))
#Compute the Variance Within Samples
within_variance = mean(c(var_x1,var_x2,var_x3))

#Compute the F test statistics
F=n*between_variance/within_variance

#Pvalue
1-pf(F,g-1,g*(n-1))

#Plot the critical region (Optional)
rejection = qf(0.95,g-1, g*(n-1))
plotDist("f", params=c(g-1, g*(n-1)), col=c("red","forestgreen"),
         groups=x >= rejection, xlim=c(0,10), ylim=c(0,1))
ladd( panel.abline(h=0,v=rejection,col='gray50') )
ladd(panel.arrows(x0 = 2, y0=0.2, y1=0, cex=0.5, x1=F, code=2))
ladd(panel.text(2,0.2),labels=paste("F=",round(0.1785,5)))

## Exercise:
#Jeff Parent is a statistics instructor who participates in triathlons. 
mile1 = c(3.15,3.24,3.23,3.22,3.21)
mile2 = c(3.19,3.22,3.21,3.17,3.19)
mile3 = c(3.34,3.31,3.29,3.31,3.29)

#Total sample size
n=length(mile1)

#Number of groups
g=3

#Means
mean_x1 = mean(mile1)
mean_x2 = mean(mile2)
mean_x3 = mean(mile3)

#Variances
var_x1 = var(mile1)
var_x2 = var(mile2)
var_x3 = var(mile3)

#Compute the Variance Between Samples
between_variance = var(c(mean_x1,mean_x2,mean_x3))
#Compute the Variance Within Samples
within_variance = mean(c(var_x1,var_x2,var_x3))

#Compute the F test statistics
F=n*between_variance/within_variance

#Critical region
rejection = qf(0.95,g-1, g*(n-1))

#Plot the critical region (Optional)
plotDist("f", params=c(g-1, g*(n-1)), col=c("red","forestgreen"),
         groups=x >= rejection, xlim=c(0,30), ylim=c(0,1))
ladd( panel.abline(h=0,v=rejection,col='gray50') )
ladd(panel.arrows(x0 = 20, y0=0.2, y1=0,  x1=27, code=2))
ladd(panel.text(2,0.2),labels=paste("F=",round(27,5)))

## Exercise:
#A large body of evidence shows that soy has health benefits for most people. Some of these benefits come largely from isoflavones, plant compounds that have estrogen-like properties. 
data = data.frame(group=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3),
                  isoflavone=c(3,17,12,10,4,19,10,9,7,5,25,15,12,9,8))
res = aov(isoflavone~group,data=data)
summary(res)


## Exercise :
#Because many HMOs either do not cover mental health costs or provide only minimal coverage, ministers and priests often need to provide counseling to persons suffering from mental illness.
library(dplyr)
cleric = data.frame(group=c(rep("Methodist",10),rep("Catholic",10),rep("Pentecostal",10)),
                  value=c(62,60,60,25,24,23,20,13,12,6,
                          62,62,24,24,22,20,19,10,8,8,
                          37,31,15,15,14,14,14,5,3,2))
#Get statistics by group
cleric %>% group_by(group) %>% summarise(mean(value))
cleric %>% group_by(group) %>% summarise(sd(value))
cleric %>% group_by(group) %>% summarise(n())
cleric %>% group_by(group) %>% summarise(median(value))

#Run the ANOVA model
res = aov(value~group,data=cleric)

#Check the results
summary(res)


