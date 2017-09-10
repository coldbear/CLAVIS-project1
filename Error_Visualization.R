## Convert order to factor variable 

str(fold_1$order)
fold_1$order <- ifelse(fold_1$order==0,0,1) # "no","yes"
fold_1$order <- factor(fold_1$order,labels = c("no","yes"))

## Split into train and test set

set.seed(222)
idx.train <- createDataPartition(fold_1$order,p=0.8,list=F)
train <- fold_1[idx.train,]
test <- fold_1[-idx.train,]

## Checking distributions

prop.table(table(train$order))
prop.table(table(fold_1$order))

## Create file to store results 

results <- setNames(as.data.frame(as.numeric(test$order)-1),"Order")
results$price <- test$price
results$revenue <- results$order*results$price

## Benchmarks

results$benchmark_0 <- results$price*0
results$benchmark_1 <- results$price*1
results$benchmark_mean <- mean(results$price)

## Creating cost matrix

zero.order <- subset(results,results$Order==0)
one.order <- subset(results,results$Order==1)

zero.order$FalsePred <- zero.order$price*1
one.order$FalsePred <- one.order$price*0

sqrt(sum((zero.order$revenue-zero.order$FalsePred)^2))
sqrt(sum((one.order$revenue-one.order$FalsePred)^2))

costs <- matrix(c(0,-2,-1,0),nrow=2,byrow=T) 

## Incorporate costs matrix in corr_mat 

# if(test$order=="no")
#   cost1 <- sqrt(sum((test$price)^2))
# 
# for(i in 1:length(results)){ 
#   if(results$Order=="no") {
#     cost1 <- sqrt(sum((results$revenue-results$price*0)^2))}
# }

## Linear Model

lr <- lm(order~.,train)
pred.lr <- predict(lr,test,type = "response") ## Predicting on test set 

a <-summary(pred.lr) ## threshold
order.lr <- ifelse(pred.lr<=a[5],"no","yes") 

x <- as.numeric(test$order)-1
y <- as.numeric(as.factor(order.lr))-1 ## For ROC

## Plot Conf_mat

cm <- confusionMatrix(test$order,order.lr)

fourfoldplot(cm$table, color = c("#0000FF","#FF0000"),
             conf.level = 0, margin = 1, main = "Threshold value -Mean")

### Code to plot interactive ROC using Shiny

devtools::install_github("hadley/ggplot2")
devtools::install_github("sachsmc/plotROC")
install.packages(c("ggplot2", "plotROC"))

library(plotROC)

shiny_plotROC()

## This will lead you to the shiny interface where you would have to upload the dataset 
## and choose the actual and predicted variables. Dataset should be <10MB. Ideally could 
## use a dataset with the predictions. 

## Look into plotting multiple curves for comparision of classifiers 
## get auc for each threshold level ??


### Lift Curve

library("ROCR")
pred <- prediction(x,y)

## ROC for comparision

perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC curve", colorize=T)

# Lift chart

perf <- performance(pred,"lift","rpp")
plot(perf, main="lift - mean", colorize=T)

## Alternate ??

install.packages("lift")
library("lift")

plotLift(pred.lr, test$order)

## Gains chart

library(ROCR)
gain.chart <- function(n) {
  score <- runif(n)
  y <- (runif(n) < score)
  plot(performance(prediction(score, y), "tpr", "rpp"),
       lwd = 7, main = paste("N =", n))
  lines(ecdf((rank(-score)[y == T]) / n),
        verticals = T, do.points = F, col = "red", lwd = 3)
}

set.seed(1)
par(mfrow = c(1, 2))
gain.chart(pred.lr)
gain.chart(10000)

## Package - Gains

install.packages("gains")
library("gains")

plot.gains(gains(x, pred.lr, groups=100,
      ties.method="average",
      conf="t", boot.reps=1000, conf.level=0.95,
      optimal=T,percents=T))
