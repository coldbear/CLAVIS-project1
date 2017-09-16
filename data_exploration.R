id <- createDataPartition(train_merged$revenue,p=0.002,list=F)
full <- train_merged[id,]
train <- readRDS("workset")
test <- readRDS("test")
rf.model<- readRDS("rf.model.ji.RDS")


### Data visualization
library(ggplot2)

# Correlation scatterplots 

ggplot(full, aes(x=full$price, y=full$price-full$competitorPrice)) + 
  geom_point(aes(col=full$order, size=full$revenue)) + 
  labs(y="Price - Competitor price", 
       x="Price ", 
       title="Effect of difference in prices on revenue")

pairs(train[,c(1,3,4,5,8)],col=train$order)
scatter.smooth(train$price,train$order)


# Missing values
install.packages("Amelia")
library(Amelia)

missmap(data, main = "Missingness Map Dataset name")
missmap(full, y.labels =NULL,y.at=NULL,col=c("black", "grey"))


