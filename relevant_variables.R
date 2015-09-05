library(randomForest)

load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/train_springleaf.Rda')
train$target <- factor(train$target)
train[is.na(train)] <- -1 # quick fix

vars <- lapply(2:(ncol(train) - 1), function(i) {
    x <- train[, i]
    a <- randomForest(data.frame(x), train$target)
    err <- mean(a$err.rate[, 1])
    res <- data.frame(var = names(train)[i], err = err,
                      c1 = a$confusion[1, 3], c2 = a$confusion[2, 3])
    print(res)
    return(res)
})