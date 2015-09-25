# Use the 'earth' package to obtain variable importance metrics.
library(earth)

# We are going to use a "low memory" technique, by reading directly
# sampled data from the .csv file directly. It will not be copied into the
# Dropbox directory as it will be too much to share. I will be reading it
# locally from the MNI computer.
#
# See https://stackoverflow.com/questions/22261082/load-a-small-random-sample-from-a-large-csv-file-into-r-data-frame
# I'm not using the R version because it is very slow. Instead, I am sampling
# the original file using Perl. 3.5 % of the file gives me around 5000 rows,
# which doesn't destroy my memory.

nsamples <- 500

varimps <- lapply(1:nsamples, function(i) {
    cat(sprintf("Sample %d/%d\n", i, nsamples))
    system("head -1 data/train.csv > data/sampled_train.csv; perl -ne 'print if (rand() < .035)' data/train.csv >> data/sampled_train.csv")
    train <- read.csv("data/sampled_train.csv", header = TRUE)
    # From the benchmarking code: replace factors with integers
    feature.names <- names(train)[2:ncol(train)-1]
    for (f in feature.names) {
      if (class(train[[f]])=="character") {
        levels <- unique(c(train[[f]], test[[f]]))
        if (length(levels) > 1) {
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
        } else {
        train[[f]] <- NULL
        test[[f]] <- NULL
        }
      }
    }

    train[is.na(train)] <- -1
    train$target <- factor(train$target)
    a <- earth(target ~ ., train, degree = 2, nfold = 10, trace = 0, 
               Use.beta.cache = FALSE)
    varimp <- evimp(a)
    print(varimp)
    cat("\n\n")
    return(varimp)
})
