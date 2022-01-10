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
  numbers = sample(list_days, size = 25, replace = TRUE)
  
  #Count equal numbers
  equal = !(length(unique(numbers)) == length(numbers))
  
  #Compute numerator
  numerator = numerator + equal
}

#Final probability
numerator/sample_size


##########   Standard Normal Distribution   ##########   
#Compute the probability of P(Z<= 1.27):
pnorm(1.27,mean=0,sd=1)

