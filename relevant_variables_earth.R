# Use the 'earth' package to obtain variable importance metrics.
library(earth)

load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/train_springleaf.Rda')
train$target <- factor(train$target)
train[is.na(train)] <- -1 # quick fix

varimps <- lapply(1:200, function(i) {
    t1 <- train[sample(20000), ]
    a <- earth(target ~ ., t1, degree = 2, nfold = 10, trace = 0)
    varimp <- evimp(a)
    print(varimp)
    return(varimp)
})