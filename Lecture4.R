####Example: Consider the following population 2,3,4,5,6,7,8,9,10,11.
population = c(2,3,4,5,6,7,8,9,10,11)

#Find all possible samples of size 2
samples = t(combn(population,2) )

#Find the mean for each sample
sample_mean = (samples[,1]+samples[,2])/2

#Plot the graph
hist(sample_mean)


####Example: Roll a die 5 times and find the proportion of odd numbers (1 or 3 or 5). What do we know about the behavior of all sample proportions that are generated as this process continues indefinitely?
dice = sample(1:6,5,replace=T)
mean(dice %in% c(1,3,5))

#Run this process 10,000 times
result = c()
for(rep in 1:10000){
  #Sample
  dice = sample(1:6,5,replace=T)
  #Store the result
  result[rep] = mean(dice %in% c(1,3,5))
}
#Plot the histogram
hist(result)


#### Example:  Consider a population of children in a family such that the ages are $\{4,5,8\}$. What do we know about the behavior of all sample means from a sample of size 2 that are generated as this process ?
#Find all possible samples of size 2
samples = expand.grid(rep(list(c(4,5,8)), 2))
#Find the mean for each sample
sample_mean = (samples[,1]+samples[,2])/2
#Plot the graph
hist(sample_mean)



#Exact probability
1-pbinom(14,100,0.2)

#Normal Approximation (no CC)
1-pnorm(-6/4)

#Normal Approximation(with CC)
1-pnorm(-1.375)






