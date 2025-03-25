setwd("D:\\Acads\\6th\\CE687A\\Assignment_1_ped_data_variables_code")

library(tidyverse)
#install.packages("corrplot")
library(corrplot)
library(dplyr)
#Import data
pedvars <- read.csv("ped_exposure_data_sample.csv")

colnames(pedvars)

# get training and test data (80-20 split)
n<-nrow(pedvars)
set.seed(12345) ##changing the seed will change the training/test data sets
index <- sample(n, round(n*0.8))
train <- pedvars[index,] #80% of data
test <- pedvars[-index,] #20% of data


###corrplot
colnames(train)
vars<-train[,6:21]
corrmat<-cor(vars, use = "complete.obs")
corrplot(corrmat, method = "circle", order = "hclust",addCoef.col = "black", tl.pos = "lt", col = COL2("RdBu", 10)) # default is circle

print(corrmat)


