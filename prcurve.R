install.packages("PRROC")
library("PRROC")


# compute PR curve
pr.2 <- pr.curve( x, y, curve=TRUE )
# and ROC curve
roc.2 <- roc.curve( x,y, curve=TRUE )

plot(pr.2, rand.plot = TRUE,max.plot = TRUE, min.plot = TRUE,
     fill.area = TRUE, fill.color = rgb(0.8,1,0.8),
     maxminrand.col = "blue" );

order.lr <- ifelse(pred.lr<=a[3],0,1)

wroc<-roc.curve(scores.class0 = res.xgb[,1], scores.class1 = res.xgb[,2],
                weights.class0 = (as.numeric(test$order)-1),
                max.compute = T, min.compute = T, rand.compute = T,curve = TRUE)

wpr<-pr.curve(scores.class0 = x, scores.class1 = x,weights.class0 = wfg,weights.class1 = 1-wfg, curve = TRUE)

library("ROCR")

pred <- prediction(order.lr,test$order)
perf <- performance(pred,"tpr","fpr")
plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)
