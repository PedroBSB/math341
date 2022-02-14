library(mosaic)
### The Texas A\&M extension service wanted to investigate if the mean yield per acre of soybeans (in bushels) was greater than 520 bushels. 

#Step 1: Compute the test statistics
z = (573-520)/(124/sqrt(36))

#Step 2: If z falls inside the critic region
region = qnorm(1-0.025)
z > region

#or Step 2: Check the probability of z under H0
1-pnorm(z)

#Step 3 (Optional): Plot the distribution under null hypothesis
plotDist("norm", params=c(520, 124/sqrt(36)), col=c("red","forestgreen"), groups=x >= 573)
ladd( panel.abline(h=0,v=573,col='gray50') )

### As a part of her evaluation of municipal employees, the city manager audits the parking tickets issued by city parking officers to determine the number of tickets that were contested by the car owner and found to be improperly issued. 

#Critical Region
rejection = qnorm(1-0.01,380,35.2/sqrt(50))

#Null Hypothesis
plotDist("norm", params=c(380, 35.2/sqrt(50)), col=c("red","forestgreen"),
         groups=x <= rejection, xlim=c(350,450), ylim=c(0,0.1))
ladd( panel.abline(h=0,v=rejection,col='gray50') )

#Alternative Hypothesis
plotDist("norm", params=c(395, 35.2/sqrt(50)), groups =  x < rejection, 
         type="h",add=TRUE,under=TRUE)
beta = pnorm(rejection,395, 35.2/sqrt(50))

#Power Curve
mu_alternative = seq(380,400,by=0.5)
power = 1-pnorm(rejection,mu_alternative, 35.2/sqrt(50))
mu_diff = mu_alternative - 380
plot(mu_diff,power,type="l")

### A massive multistate outbreak of foodborne illness was attributed to Salmonella enteritidis.
x=c(.593,.142,.329,.691,.231,.793,.519,.392,.418)
mean(x)
sd(x)


### An airline wants to evaluate the depth perception of its pilots over the age of 50. 
x=c(2.7,2.4,1.9,2.6,2.4,1.9,2.3,2.2,2.5,2.3,1.8,2.5,2.0,2.2)
x_bar = mean(x)
s = sd(x)
alpha = 0.05
n=length(x)
lower_limit = x_bar-qt(1-alpha/2,n-1)*s/sqrt(n)
upper_limit = x_bar+qt(1-alpha/2,n-1)*s/sqrt(n)

