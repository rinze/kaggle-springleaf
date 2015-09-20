library(caret)

load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/train_springleaf.Rda')
train$target <- factor(train$target, labels = c("Negative", "Positive"))
train[is.na(train)] <- -1 # quick fix


#train <- train[, c("target", "VAR_0711", "VAR_1029", "VAR_1827", "VAR_1392",
#                   "VAR_1121", "VAR_1580", "VAR_1748", "VAR_0417", "VAR_0815")]

train <- train[, c("target", "VAR_0088", "VAR_1127", "VAR_0810", "VAR_0880", "VAR_1442")]

model1 <- train(target ~ ., 
                train, 
                method = "gbm", 
                trControl = trainControl(method = "cv", 
                                            number = 10,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE), 
                tuneGrid = expand.grid(interaction.depth = 1:4, 
                                         n.trees = c(50, 100, 200, 300), 
                                         shrinkage = c(0.1, 0.2, 0.3), 
                                         n.minobsinnode = c(10, 20, 50, 100)),
                metric = "ROC")

train <- NULL
gc()

