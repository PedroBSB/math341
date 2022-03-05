#Call libraries
library(tidyr)
library(ggplot2)

#Generate a sample
set.seed(1234)
algo1 = rnorm(100,15,3)
algo2 = rnorm(100,15,3)
algo3 = rnorm(100,18,3)

#Build the dataframe
df = data.frame(algo1,algo2,algo3)
df = round(df,2)
df_panel = df %>% gather()

#Plot the histogram
ggplot(df_panel, aes(x = value, fill = key)) +                       
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) + theme_bw()

#Compute the ANOVA
options(digits=10)
res = aov(value~key,data=df_panel)
summary(res)

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


