### RANDOM FOREST ###
rm(list = ls())

# sources
source("DataPrep.R")
source("helper.apaproject.R")

# take data partition
idx <- createDataPartition(y = fold_1$order, p = 0.2, list = FALSE)
train <- fold_1[idx,]
test <- fold_1[-idx,]

# Changing col-names for randomForest
# randomForest package has problems with columns starting with a number, so I changed all col names
train_col <- train
test_col <- test
colnames(train_col) <- paste("col", colnames(train_col), sep = "_")
colnames(test_col) <- paste("col", colnames(test_col), sep = "_")

## TRAINING MODEL 
# classification
train_col$col_order <- as.factor(train_col$col_order)

#basic
# randomForest
rf <- randomForest(col_order ~ ., data=train_col,
                   importance=TRUE,
                  ntree = 500)

#rfsrc
rf.rfsrc <- rfsrc(col_order ~ ., data = train_col)

# methods
gg_dta <- gg_rfsrc(rf.rfsrc) # predictions
# gg_error <- gg_error(rf.rfsrc) # doesnt work
rfsrc.roc <- gg_roc(rf.rfsrc, which.outcome = 1)
rfsrc.roc2 <- gg_roc(rf.rfsrc, which.outcome = 2)
rfsrc.vimp <- gg_vimp(rf.rfsrc)


# plotting
plot(gg_dta) # predictions
# plot(gg_error) # doesnt work
plot(rfsrc.roc)
plot(rfsrc.roc2)
plot(rfsrc.vimp)


## PREDICTING 
pred.rf <- predict(rf, newdata = test_col, type = "prob")[,2]

## MODEL INTERPRETATION
## classical measurements
auc.rf <- auc(test$col_order, pred.rf) 
roc.rf <-roc(test$col_order, pred.rf)
plot.roc(roc.rf)


