library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(doMC)

registerDoMC(cores = 4)
setwd("~/Projects/kaggle/plankton")
p <- read.csv("plankton_train_48.csv", header = TRUE, stringsAsFactors = TRUE)
plankton <- tbl_df(p[-c(1, 2, 3)])

set.seed(123)
inTrain <- createDataPartition(plankton$y, p = 0.75, list = FALSE)

training <- plankton[inTrain, ]
testing <- plankton[-inTrain, ]

tc <- trainControl(method = "cv", number = 3, allowParallel = TRUE)
startTime <- proc.time()
fit <- train(y ~ ., data = training, method = "rf", trControl = tc, prox = FALSE)
duration <- proc.time() - startTime
duration
#      user    system   elapsed
# 30648.801   240.166 23252.554

prediction <- predict(fit, newdata = testing, type = "prob")
confMat <- confusionMatrix(testing$y, prediction)
confMat$overall
confMat$byClass
confMat$byClass["Class: stomatopod", ]
confMat$byClass[confMat$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat$byClass[, "Balanced Accuracy"]), ]
misclassified <- confMat$byClass[confMat$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat$byClass[, "Balanced Accuracy"]), ]

prediction[1, prediction[1,] > max(prediction[1, ]) - max(prediction[1, ])/2]
prediction[3, prediction[3,] > max(prediction[3, ]) - max(prediction[3, ])/2]

# Overall Statistics

#                Accuracy : 0.4253
#                  95% CI : (0.4141, 0.4365)
#     No Information Rate : 0.0809
#     P-Value [Acc > NIR] : < 2.2e-16

#                   Kappa : 0.4095
#  Mcnemar's Test P-Value : NA

predict(fit, testCases)

startTime2 <- proc.time()
fit2 <- train(y ~ s.area + s.perimeter + s.radius.mean + s.radius.sd + s.radius.min + s.radius.max + m.cx + m.cy + m.majoraxis + m.eccentricity + m.theta, data = training, method = "rf", trControl = tc, prox = FALSE)
duration2 <- proc.time() - startTime2
prediction2 <- predict(fit2, newdata = testing, type = "prob")
confMat2 <- confusionMatrix(testing$y, prediction2)
confMat2$byClass[confMat2$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat2$byClass[, "Balanced Accuracy"]), ]
misclassified2 <- confMat2$byClass[confMat2$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat2$byClass[, "Balanced Accuracy"]), ]

# Overall Statistics

#                Accuracy : 0.4408
#                  95% CI : (0.4295, 0.4521)
#     No Information Rate : 0.0792
#     P-Value [Acc > NIR] : < 2.2e-16

#                   Kappa : 0.4253
#  Mcnemar's Test P-Value : NA

save(fit4, file = "fit_48.RData")
load("fit_04.RData")

# Using Random Forest
library(randomForest)

startTime <- proc.time()
# fit3 <- randomForest(y ~ s.area + s.perimeter + s.radius.mean + s.radius.sd + s.radius.min + s.radius.max + m.cx + m.cy + m.majoraxis + m.eccentricity + m.theta, data = training)
# fit4 <- randomForest(y ~ ., data = training)
fit4 <- randomForest(y ~ ., data = training[-c(1, 2, 3)])
duration <- proc.time() - startTime
duration

prediction2 <- predict(fit4, newdata = testing, type = "prob")

importance(fit4)
table(fit4$predicted == training$y)

prediction <- predict(fit4, type = "prob")
ymin <- 1/1000
prediction[prediction < ymin] <- ymin

mcloss(training$y, prediction)

mcloss <- function (y_actual, y_pred) {
  dat <- rep(0, length(y_actual))
  for(i in 1:length(y_actual)){
    dat_x <- y_pred[i,y_actual[i]]
    dat[i] <- min(1-1e-15,max(1e-15,dat_x))
  }
  return(-sum(log(dat))/length(y_actual))
}


