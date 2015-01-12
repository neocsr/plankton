library(caret)
library(ggplot2)
library(doMC)

registerDoMC(cores = 2)
setwd("~/Projects/kaggle/National Data Science Bowl")
plankton <- read.csv("plankton.csv", header = TRUE, stringsAsFactors = TRUE)

set.seed(123)
inTrain <- createDataPartition(plankton$y, p = 0.75, list = FALSE)

training <- plankton[inTrain, ]
testing <- plankton[-inTrain, ]

tc <- trainControl(method = "cv", number = 3, allowParallel = TRUE)
startTime <- proc.time()
fit <- train(y ~ ., data = training, method = "rf", trControl = tc, prox = FALSE)
duration <- proc.time() - startTime
#      user    system   elapsed
# 30648.801   240.166 23252.554

prediction <- predict(fit, newdata = testing)
confMat <- confusionMatrix(testing$y, prediction)
confMat$overall
confMat$byClass
confMat$byClass["Class: stomatopod", ]
confMat$byClass[confMat$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat$byClass[, "Balanced Accuracy"]), ]
misclassified <- confMat$byClass[confMat$byClass[, "Balanced Accuracy"] < 0.5 & !is.na(confMat$byClass[, "Balanced Accuracy"]), ]

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
prediction2 <- predict(fit, newdata = testing)
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

save(fit4, file = "fit_04.RData")
load("fit_02.RData")

# Using Random Forest
library(randomForest)

startTime <- proc.time()
# fit3 <- randomForest(y ~ s.area + s.perimeter + s.radius.mean + s.radius.sd + s.radius.min + s.radius.max + m.cx + m.cy + m.majoraxis + m.eccentricity + m.theta, data = training)
fit4 <- randomForest(y ~ ., data = training)
duration <- proc.time() - startTime

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


