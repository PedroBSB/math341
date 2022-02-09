library(mosaic)
#The Texas A\&M extension service wanted to investigate if the mean yield per acre of soybeans (in bushels) was greater than 520 bushels. 

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
