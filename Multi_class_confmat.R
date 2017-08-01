### Convert items to factor

install.packages("fancycut")
library(fancycut)

table(fold_1$items)

fold_1$items <- fancycut(fold_1$items,c('[0,0]','[1,4]','[5,10]','[12,50]'),
                    c('None','Low','Med','High'),unmatched.bucket = 'Med',
                    out.as.factor = T)

summary(fold_1$items)

fold_1$items <- droplevels(fold_1$items)
fold_1$items <- factor(fold_1$items)
fold_1$order = NULL


## Split into train and test set

set.seed(222)
idx.train <- createDataPartition(fold_1$items,p=0.8,list=F)
train <- fold_1[idx.train,]
test <- fold_1[-idx.train,]

## Checking distributions

prop.table(table(train$items))
prop.table(table(fold_1$items))

## Models 

### Multinomial regression

library("nnet")

model <- multinom(items~.,train)
pred.mod <- predict(model,test)

# Confusion matrix

confmat <- caret::confusionMatrix(pred.mod,test$items)
a <- as.data.frame(confmat$table)

## Gradient Boosting

model.control<- trainControl(
  method = "cv", # cross validation
  number = 5, # number of folds in cross validation
  allowParallel = TRUE,
  classProbs = T
)

xgb.parms.opt <- expand.grid(nrounds = 400, 
                             max_depth = 10, 
                             eta = 0.05, 
                             gamma = 0,
                             colsample_bytree = 1,
                             min_child_weight = 1, 
                             subsample = 0.6)

xgb <- train(order~., data = train,  
             method = "xgbTree",
             tuneGrid = xgb.parms.opt,
             metric = "ROC", trControl = model.control)

pred.xgb <- predict(xgb,test)

c.xgb <- caret::confusionMatrix(pred.xgb,test$items)

#### Plot conf mat as a heatmap - basic 

tile <- ggplot() +
  geom_tile(aes(x=Prediction, y=Reference,fill=Freq),data=a, color="black",size=0.1) +
  labs(x="Actual",y="Predicted")

tile = tile + 
  geom_text(aes(x=Prediction,y=Reference, label=sprintf("%.1f", Freq)),data=a, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

tile = tile + 
  geom_tile(aes(x=Prediction,y=Reference),data=subset(a, as.character(Prediction)==as.character(Reference)),
            color="black",size=0.3, fill="black", alpha=0)

tile

#### Interactive heatmaps

install.packages("d3heatmap")
library("d3heatmap")

d3heatmap(confmat$table,scale="row",dendrogram="none", colors ="Blues") #scales::col_quantile("Blues",NULL,5)
