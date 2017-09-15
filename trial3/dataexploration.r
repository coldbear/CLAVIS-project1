
smp_size <- floor(0.7* nrow(data))

saveRDS(test, "test")
saveRDS(train, "train")
## set the seed to make your partition reproductible
set.seed(123)
idx <- createDataPartition(data$order,p = 0.7, list = FALSE)


train <- data[idx, ]
test <- data[-idx, ]

rf.model <- randomForest(order~., data=train, ntree=1000, mtry=6, importance=TRUE)
rf.model
saveRDS(rf.model, "rf.model")
