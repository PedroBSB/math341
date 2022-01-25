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

########## Brand Recognition
consumers = rbinom(50,size=1,prob=0.95) #1 means recognize the brand

########## Shaquille O’Neal
next_free_throw = rbinom(1,size=1,prob=0.527) #1 means The free throw is made



