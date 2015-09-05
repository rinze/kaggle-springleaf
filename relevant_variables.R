library(randomForest)

load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/train_springleaf.Rda')
train$target <- factor(train$target)
train[is.na(train)] <- -1 # quick fix

vars <- lapply(2:(ncol(train) - 1), function(i) {
    x <- train[, i]
    a <- randomForest(data.frame(x), train$target)
    err <- mean(a$err.rate[, 1])
    cat(sprintf("Error for %s: %.5f\n", names(train)[i], err))
    return(data.frame(var = names(train)[i], err = err))
})