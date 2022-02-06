##########   Same birthday problem   ##########   

#First step: Define the sample size of the simulation
sample_size = 1000

#Generate 25 random numbers between 1 and 365
list_days = seq(1,365)
numbers = sample(list_days, size = 25, replace = TRUE)

#Count equal numbers
equal = !(length(unique(numbers)) == length(numbers))

#Create a loop
numerator = 0
for(sample in 1:sample_size){
  #Generate 25 random numbers between 1 and 365
  list_days = seq(1,365)
  numbers = sample(list_days, size = 11, replace = TRUE)
  
  #Count equal numbers
  equal = !(length(unique(numbers)) == length(numbers))
  
  #Compute numerator
  numerator = numerator + equal
}

#Final probability
numerator/sample_size

<<<<<<< HEAD
########## Brand Recognition
consumers = rbinom(50,size=1,prob=0.95) #1 means recognize the brand

########## Shaquille O’Neal
next_free_throw = rbinom(1,size=1,prob=0.527) #1 means The free throw is made



=======
#McDonalds
rbinom(1000,1,0.95)

#Shaquille O'Neal
rbinom(1,1,0.527)



##########   Standard Normal Distribution   ##########   
#Compute the probability of P(Z<= 1.27):
pnorm(1.27,mean=0,sd=1)


#Compute the probability of P(Z> -1):
pnorm(-1,mean=0,sd=1,lower.tail = FALSE)
#Or
1-pnorm(-1,mean=0,sd=1)

#Compute the probability of P(-2.50 <= Z <= -1) = P(Z <= -1) - P(Z<=-2.50):
pnorm(-1,mean=0,sd=1) - pnorm(-2.5,mean=0,sd=1)

#Find the percentile P95 of a standard normal distribution
qnorm(0.95,mean=0,sd=1)

#Find the value of Z_{0.025}
qnorm(1-0.025,mean=0,sd=1)
>>>>>>> 819b8b643e9f42cafa93207ece2f27e5baa5e5f9
