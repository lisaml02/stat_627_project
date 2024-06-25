inst <- read_csv("../downloads/inst_clean.csv")
View(inst)

library(tidyverse)
library(class)
library(caret)
library(ISLR2)

set.seed(12345)  
training_pct <- 0.8
Z <- sample(nrow(inst), floor(training_pct*nrow(inst)))
inst.training <- inst[Z, ]
inst.testing <- inst[-Z, ]
c(nrow(inst), nrow(inst.training), nrow(inst.testing))


#Multiple linear regression

linear_training <- lm(ASSET + EQ + ROA + ROE ~ BKCLASS + DEP + DEPDOM + NETINC + ROAPTX + ROAPTXQ + ROAQ + STNAME, data = inst.training)
linear_training
summary(linear_training)$coefficients[, "Pr(>|t|)"]

#Testing

linear_testing <- lm(ASSET + EQ + ROA + ROE ~ BKCLASS + DEP + DEPDOM + NETINC + ROAPTX + ROAPTXQ + ROAQ + STNAME, data = inst.testing)
linear_testing
summary(linear_testing)$coefficients[, "Pr(>|t|)"]

#Ridge Regression

predictors_ridge_training <- data.matrix(inst.training[, c("BKCLASS", "DEP", "DEPDOM", "EQ", "NETINC", "ROA", "ROAPTX", "ROAPTXQ", "ROAQ", "ROE", "STNAME")])
scaled_training <- scale(predictors_ridge_training)
response_ridge_training <- data.matrix(inst.training[,c("ASSET")])
model_ridge_training <- glmnet(scaled_training,response_ridge_training, alpha = 0)
cv_training <- cv.glmnet(scaled_training,response_ridge_training, alpha = 0)
lambda_training <- cv_training$lambda.min
ridge_training <- glmnet(scaled_training,response_ridge_training, alpha=0, lambda=lambda_training)
summary(ridge_training)
y_predicted_training <- predict(ridge_training, newx = scaled_training)
MSE_training <- mean((response_ridge_training - y_predicted_training)^2)
MSE_training
SST_training <- sum((response_ridge_training - mean(response_ridge_training))^2)
SSE_training <- sum((response_ridge_training - y_predicted_training)^2)
R_squared_training <- 1 - (SSE_training / SST_training)
SST_training
SSE_training
R_squared_training
ridge_training[["df"]]

#Testing

predictors_ridge_testing <- data.matrix(inst.testing[, c("BKCLASS", "DEP", "DEPDOM", "EQ", "NETINC", "ROA", "ROAPTX", "ROAPTXQ", "ROAQ", "ROE", "STNAME")])
scaled_testing <- scale(predictors_ridge_testing)
response_ridge_testing <- data.matrix(inst.testing[,c("ASSET")])
model_ridge_testing <- glmnet(scaled_testing,response_ridge_testing, alpha = 0)
cv_testing <- cv.glmnet(scaled_testing,response_ridge_testing, alpha = 0)
lambda_testing <- cv_testing$lambda.min
ridge_testing <- glmnet(scaled_testing,response_ridge_testing, alpha=0, lambda=lambda_testing)
summary(ridge_testing)
y_predicted_testing <- predict(ridge_testing, newx = scaled_testing)
MSE_testing <- mean((response_ridge_testing - y_predicted_testing)^2)
MSE_testing
SST_testing <- sum((response_ridge_testing - mean(response_ridge_testing))^2)
SSE_testing <- sum((response_ridge_testing - y_predicted_testing)^2)
R_squared_testing <- 1 - (SSE_testing / SST_testing)
SST_testing
SSE_testing
R_squared_testing
ridge_testing[["df"]]
