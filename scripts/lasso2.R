rm(list = objects())
library(tidyverse)
library(xgboost)
library(mgcv)
library(RSpectra)
library(glmnet)

#################################################################################### import training data, parsing, replace NA
getwd()
Data <- read_csv("Data/train.csv", col_names = T)
loss <- function(y, ychap) {
    sqrt(mean((log(ychap + 1) - log(y + 1))^2))
}

parsing_failures <- problems(Data)
columns <- parsing_failures[, 2] %>% unlist
for (i in c(1:length(columns))) {
    Data[which(is.na(Data[, columns[i]])), columns[i]] <- mean(Data[, columns[i]] %>% unlist %>% 
        as.numeric, na.rm = T)
}

#################################################################################### drop variables with only 0

nb_zero <- function(x) {
    sum(x == 0)
}


nzero <- lapply(Data, nb_zero)
Data <- Data[, -which(nzero %>% unlist == nrow(Data))]


#################################################################################### lasso to select a subset of variables
n <- nrow(Data)
N <- n/2
set.seed(150)
s <- sample(c(1:n), N)

dataxgb0 <- as.matrix(Data[s, -c(1, 2)])
dataxgb1 <- as.matrix(Data[-s, -c(1, 2)])


glm0 <- glmnet(x = dataxgb0, y = log(Data[s, ]$target), family = c("gaussian"), offset = NULL, 
    alpha = 1, nlambda = 100, standardize = TRUE, intercept = T)

plot(glm0, label = T, xvar = "lambda")

glm0$beta

glm0cv <- cv.glmnet(x = dataxgb0, y = log(Data[s, ]$target), family = c("gaussian"), offset = NULL, 
    alpha = 1, nlambda = 100, standardize = TRUE, intercept = TRUE)
plot(glm0cv)
glm0cv$lambda.min

l <- array(0, dim = ncol(glm0$beta))
for (i in c(1:ncol(glm0$beta))) {
    ychap <- exp(dataxgb1 %*% as.matrix(glm0$beta[, i]) + glm0$a0[i])
    l[i] <- loss(y = Data[-s, ]$target, ychap = ychap)
}

plot(l[1:40], type = "b")

glm0$lambda[which.min(l)]

min(l)

plot(glm0$beta[, which.min(l)], type = "b", pch = 20)
rownames(glm0$beta)[which(glm0$beta[, which.min(l)] != 0)]


`?`(predict)



########################################################################################################################### regression on selected variables

reg0 <- lm(log(Data[s, ]$target) ~ dataxgb0[, which(glm0$beta[, which.min(l)] != 0)])
summary(reg0)

reg0.forecast <- exp(reg0$coefficients[1] + dataxgb1[, which(glm0$beta[, which.min(l)] != 
    0)] %*% reg0$coefficients[-1])

loss(y = Data[-s, ]$target, ychap = reg0.forecast)
loss(y = Data[-s, ]$target, ychap = exp(reg0$coefficients[1]))


### le lasso marche mieux!


########################################################################################################################### boosting on selected variables

dataxgb0_1 <- dataxgb0[, which(glm0$beta[, which.min(l)] != 0)]
dataxgb1_1 <- dataxgb1[, which(glm0$beta[, which.min(l)] != 0)]

xgb0 <- xgboost(params = list(subsample = 0.5, eta = 0.05, max.depth = 6, colsample_bytree = 1), 
    data = dataxgb0_1, label = log(Data[s, ]$target), , nthread = 4, objective = "reg:linear", 
    nround = 1000, booster = "gbtree", verbose = 0)

xgb0.fitted <- predict(xgb0, dataxgb0_1) %>% exp
xgb0.forecast <- predict(xgb0, dataxgb1_1) %>% exp

loss(y = Data[s, ]$target, ychap = xgb0.fitted)
loss(y = Data[-s, ]$target, ychap = xgb0.forecast)


which(glm0$beta[, which.min(l)] != 0)






##### random becnhmark

dataxgb0_1 <- dataxgb0[, sample(c(1:ncol(dataxgb0)), 10)]
dataxgb1_1 <- dataxgb1[, sample(c(1:ncol(dataxgb0)), 10)]

xgb0 <- xgboost(params = list(subsample = 0.5, eta = 0.05, max.depth = 6, colsample_bytree = 1), 
    data = dataxgb0_1, label = log(Data[s, ]$target), , nthread = 4, objective = "reg:linear", 
    nround = 1000, booster = "gbtree", verbose = 0)

xgb0.fitted <- predict(xgb0, dataxgb0_1) %>% exp
xgb0.forecast <- predict(xgb0, dataxgb1_1) %>% exp

loss(y = Data[s, ]$target, ychap = xgb0.fitted)
loss(y = Data[-s, ]$target, ychap = xgb0.forecast)





########################################################################################################################### submission
dataxgb0 <- as.matrix(Data[, -c(1, 2)])
dataxgb0 <- dataxgb0[, which(glm0$beta[, which.min(l)] != 0)]


xgb0 <- xgboost(params = list(subsample = 0.5, eta = 0.05, max.depth = 6, colsample_bytree = 1), 
    data = dataxgb0, label = log(Data$target), , nthread = 4, objective = "reg:linear", nround = 1000, 
    booster = "gbtree", verbose = 0)

xgb0.fitted <- predict(xgb0, dataxgb0) %>% exp
loss(y = Data$target, ychap = xgb0.fitted)


DataTest <- read_csv("Data/test.csv", col_names = T)
dataxgb1 <- as.matrix(DataTest[, -1])
dataxgb1 <- dataxgb1[, which(glm0$beta[, which.min(l)] != 0)]

xgb0.forecast <- predict(xgb0, dataxgb1) %>% exp
sub <- read_csv("Data/sample_submission-2.csv", col_names = T)

sub_forecast <- data.frame(ID = DataTest$ID, forecast = xgb0.forecast)
sub_final <- left_join(sub, sub_forecast, by = "ID")
sub_final <- data.frame(ID = sub_final$ID, target = sub_final$forecast)


write_csv(sub_final, "Submissions/sub2.csv")

