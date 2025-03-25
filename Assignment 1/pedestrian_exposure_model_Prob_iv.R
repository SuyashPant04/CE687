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

# Extract pedestrian volumes for District 4 and District 7 (Full Data)
d4_ped <- pedvars %>% filter(District == 4) %>% pull(AnnualEst)
d7_ped <- pedvars %>% filter(District == 7) %>% pull(AnnualEst)

# Conduct Welch’s two-sample t-test (Full Data)
t_test_result <- t.test(d4_ped, d7_ped, alternative = "two.sided", var.equal = FALSE)

# Print test results
print(t_test_result)
