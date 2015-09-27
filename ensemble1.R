# Ensemble best variables

library(caret)
library(caretEnsemble)

#### LOAD data ####
load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/train_springleaf.Rda')
train$target <- factor(train$target, labels = c("Negative", "Positive"))
target <- train$target
train$target <- NULL

# Get near zero variance variables (varible nzv)
load("C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/processed_data/nzv.Rda")
nzv_vars <- names(train[, nzv])
train <- train[, -nzv]
train$ID <- NULL

# GEt highly correlated variables (variable cor_vars)
load("C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/processed_data/cor_vars.Rda")
train <- train[, !names(train) %in% cor_vars]


# From manual data analysis
datecolumns = c("VAR_0075", "VAR_0204", "VAR_0217")

# Convert factors to integers for the xgboost algorithm

fac <- sapply(train, class)
fac <- names(train[, fac == "factor"])
fac <- fac[!fac %in% datecolumns]
for (f in fac) {
    train[, f] <- as.numeric(train[, f])
}

# big_factors <- sapply(train[sapply(train, class) == "factor"], function(x) length(levels(x)) > 52)
# big_factors <- names(big_factors[big_factors == TRUE])
# big_factors <- big_factors[!big_factors %in% datecolumns]
# 
# for (f in big_factors) {
#         train[, f] <- as.numeric(train[, f])
# }

## Numeric and integers, see correlations
# temp <- train[, sapply(train, class) %in% c("integer", "numeric")]
# ctemp <- cor(temp)
# fc <- findCorrelation(temp)
# cor_vars <- names(temp[fc])
# save(cor_vars, 
#      file = "C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/processed_data/cor_vars.Rda")

#### CONFIG ####


TESTING <- FALSE

if (TESTING) {
    idx <- sample(nrow(train), 500)
    target <- target[idx]
    train <- train[idx, ]
    trctrl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)
} else {
    trctrl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           savePredictions = TRUE)
}


#### MAIN ####



# Process date variables (copy & paste from existing script on Kaggle scripts)

# And we also need to check variables that contain "weird" values, such as 9999999 and so.

train[train > 999999990] <- -1
train[train == 9998] <- -1
train[train == 9997] <- -1
train[train == 9996] <- -1
train[train == 998] <- -1
train[train == 997] <- -1
train[train == 996] <- -1
train[train == 98] <- -1
train[train == 97] <- -1
train[train == 96] <- -1

train_cropped <- train[datecolumns]
train_cc <- data.frame(apply(train_cropped, 2, 
                             function(x) as.double(strptime(x, 
                                                            format='%d%b%y:%H:%M:%S',
                                                            tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
    train[dc] <- NULL
    train[dc] <- train_cc[dc]
}

train_cc <- NULL
train_cropped <- NULL
gc()

train[is.na(train)] <- -1 # quick fix


# Let's use the datecolumns and the variables we have identified as relevant
# goodvars <- c(datecolumns, "VAR_0088", "VAR_1127", "VAR_0810", "VAR_0880", 
#               "VAR_1442", "VAR_1128", "VAR_0795")


cat("Building models... ")
# Train all the models using an ensemble
allmodels <- caretList(x = train, y = target,
                       trControl = trctrl,
                       tuneList = list(
                           svmLinear = caretModelSpec(method = "svmLinear",
                                                      tuneGrid = expand.grid(C = c(1, 2, 5, 10))),
                           xgboost = caretModelSpec(method = "xgbLinear",
                                                    tuneGrid = expand.grid(nrounds = 900,
                                                                           lambda = c(0.1, 0.2, 0.3, 0.4),
                                                                           alpha = c(0.1, 0.2, 0.3, 0.4))),
                           ada = caretModelSpec(method = "ada", tuneLength = 4),
                           gbm = caretModelSpec(method = "gbm", verbose = FALSE, tuneLength = 4),
                           rf = caretModelSpec(method = "rf")
                       ),
                       metric = "ROC"
)
cat("OK\n")

cat("Ensembling... ")
ensemble <- caretEnsemble(allmodels)
cat("OK\n")

cat("Stacking... ")
stack <- caretStack(allmodels, method = "ada", 
                    trControl = trainControl(classProbs = TRUE,
                                             summaryFunction = twoClassSummary),
                    metric = "ROC")
cat("OK\n")

cat("Processing test... ")
used_vars <- names(train)
train <- NULL
gc()
load('C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/raw_data/test_springleaf.Rda')
ID <- test$ID
test <- test[, used_vars]

fac <- sapply(test, class)
fac <- names(test[, fac == "factor"])
fac <- fac[!fac %in% datecolumns]
for (f in fac) {
    test[, f] <- as.numeric(test[, f])
}

test[test > 999999990] <- -1
test[test == 9998] <- -1
test[test == 9997] <- -1
test[test == 9996] <- -1
test[test == 998] <- -1
test[test == 997] <- -1
test[test == 996] <- -1
test[test == 98] <- -1
test[test == 97] <- -1
test[test == 96] <- -1

test_cropped <- test[datecolumns]
test_cc <- data.frame(apply(test_cropped, 2, 
                             function(x) as.double(strptime(x, 
                                                            format='%d%b%y:%H:%M:%S',
                                                            tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
    test[dc] <- NULL
    test[dc] <- test_cc[dc]
}

test_cc <- NULL
test_cropped <- NULL
gc()

test[is.na(test)] <- -1 # quick fix

pred_ensemble <- predict(ensemble, newdata = test)
pred_stack <- predict(stack, newdata = test, type = "prob")

pred_ensemble <- data.frame(ID = ID, target = pred_ensemble)
pred_stack <- data.frame(ID = ID, target = pred_stack[, 2])

result_ensemble <- sprintf("C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/processed_data/sl_ensemble_%s.csv.gz", 
                           gsub(":", "", gsub(" ", "_", Sys.time())))
result_stack <- sprintf("C:/Users/jmmateos/Dropbox/Yashar_Jose_Mahsa/Springleaf_Marketing_Response/processed_data/sl_stack_%s.csv.gz", 
                           gsub(":", "", gsub(" ", "_", Sys.time())))

gzresultfile <- gzfile(result_ensemble)
write.csv(pred_ensemble, file = gzresultfile, quote = FALSE, row.names = FALSE)
gzresultfile <- gzfile(result_stack)
write.csv(pred_stack, file = gzresultfile, quote = FALSE, row.names = FALSE)

cat("OK\n")

