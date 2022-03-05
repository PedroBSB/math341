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
