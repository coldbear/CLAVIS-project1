
original <- readRDS("olddata")

explora1 <- function(orignal, train){
# Correlation scatterplots 

ggplot(original, aes(x=original$price, y=original$price-original$competitorPrice)) + 
  geom_point(aes(col=original$order, size=original$revenue)) + 
  labs(y="Price - Competitor price", 
       x="Price ", 
       title="Effect of difference in prices on revenue")
}

explora2 <- function(train){
pairs(train[,c(1,3,4,5,8)],col=train$order)
}
#e <- scatter.smooth(train$price,train$order)



