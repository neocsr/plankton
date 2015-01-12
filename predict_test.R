library(reshape2)

categories <- list.dirs(path = "./competition_data/train", full.names = FALSE, recursive = TRUE)
categories <- categories[-1]
dummy <- rep("x.jpg", length(categories))
dummies <- data.frame(image = dummy, prediction = factor(categories))

testCases <- read.csv("test_cases.csv", header = TRUE, stringsAsFactors = FALSE)

prediction <- predict(fit4, newdata = testCases, type = "prob")
testCases$prediction <- prediction

summary <- rbind(testCases[, c("image", "prediction")], dummies)

submission <- dcast(summary, image ~ prediction)
submission[, -1] <- (0 + !is.na(submission[, -1]))


submission <- cbind(testCases$image, prediction)
colnames(submission)[1] <- "image"

write.csv(submission, "./submission.csv", col.names = TRUE, row.names = FALSE, quote = FALSE)
