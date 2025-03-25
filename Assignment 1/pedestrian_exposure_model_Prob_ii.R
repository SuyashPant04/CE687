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

#For Problem 2

volume_by_district <- pedvars %>%
  group_by(District) %>%
  summarize(
    n = n(),
    mean_AADT = mean(AnnualEst),
    sd_AADT = sd(AnnualEst),
    se_AADT = sd_AADT / sqrt(n),  # Standard Error
    ci_lower = mean_AADT - qt(0.975, df = n-1) * se_AADT,  # 95% CI lower bound
    ci_upper = mean_AADT + qt(0.975, df = n-1) * se_AADT   # 95% CI upper bound
  ) %>%
  as.data.frame()

# Print the result
print(volume_by_district)
