setwd("D:\\Acads\\6th\\CE687A\\Assignment_1_ped_data_variables_code")

library(tidyverse)
#install.packages("corrplot")
#library(corrplot)
library(dplyr)
library(car)
#Import data
pedvars <- read.csv("ped_exposure_data_sample.csv")

colnames(pedvars)

# get training and test data (80-20 split)
n<-nrow(pedvars)
set.seed(12345) ##changing the seed will change the training/test data sets
index <- sample(n, round(n*0.8))
train <- pedvars[index,] #80% of data
test <- pedvars[-index,] #20% of data

##########Initial models#########
#Modeling AnnualEst#########
model_1 <- lm(AnnualEst ~ PopT + WalkComT + EmpT + Signal + Int4way,
              data=train)
summary(model_1)


#Modeling logAnnualEst#########
model_2 <- lm(logAnnualEst ~ PopT + WalkComT + EmpT + Signal + Int4way,
              data=train)
summary(model_2)

# Extended Linear Model 3 (Including HseHldT)
model_3 <- lm(AnnualEst ~ PopT + WalkComT + EmpT + Signal + Int4way + HseHldT, data = train)
summary(model_3)

# Extended Log-Linear Model 4 (Including HseHldT)
model_4 <- lm(logAnnualEst ~ PopT + WalkComT + EmpT + Signal + Int4way + HseHldT, data = train)
summary(model_4)

# Model Performance Metrics
# Compute Log-Likelihood, AIC, and BIC for each model
logLik(model_1); AIC(model_1); BIC(model_1)
logLik(model_2); AIC(model_2); BIC(model_2)
logLik(model_3); AIC(model_3); BIC(model_3)
logLik(model_4); AIC(model_4); BIC(model_4)

# RMSE Calculation
# Predictions on Test Data
AnnualEst_pred_model_1 <- predict(model_1, test)
AnnualEst_pred_model_2 <- exp(predict(model_2, test))
AnnualEst_pred_model_3 <- predict(model_3, test)
AnnualEst_pred_model_4 <- exp(predict(model_4, test))

# RMSE Calculation
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Compute RMSE for Training and Testing
model_1_rmse_train <- rmse(train$AnnualEst, predict(model_1, train))
model_1_rmse_test <- rmse(test$AnnualEst, AnnualEst_pred_model_1)

model_2_rmse_train <- rmse(train$AnnualEst, exp(predict(model_2, train)))
model_2_rmse_test <- rmse(test$AnnualEst, AnnualEst_pred_model_2)

model_3_rmse_train <- rmse(train$AnnualEst, predict(model_3, train))
model_3_rmse_test <- rmse(test$AnnualEst, AnnualEst_pred_model_3)

model_4_rmse_train <- rmse(train$AnnualEst, exp(predict(model_4, train)))
model_4_rmse_test <- rmse(test$AnnualEst, AnnualEst_pred_model_4)

# Print RMSE values
rmse_values <- data.frame(
  Model = c("Linear Model 1", "Log-Linear Model 2", "Extended Linear Model 3", "Extended Log-Linear Model 4"),
  Train_RMSE = c(model_1_rmse_train, model_2_rmse_train, model_3_rmse_train, model_4_rmse_train),
  Test_RMSE = c(model_1_rmse_test, model_2_rmse_test, model_3_rmse_test, model_4_rmse_test)
)
print(rmse_values)

# Summary of Signal Impact on Pedestrian Volume
volume_by_signal <- train %>%
  group_by(Signal) %>%
  summarize(n = n(), mean_AADT = mean(AnnualEst), sd_AADT = sd(AnnualEst)) %>%
  as.data.frame()
print(volume_by_signal)

