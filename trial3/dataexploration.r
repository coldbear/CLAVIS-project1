
original <- readRDS("original.RDS")

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

explora2 <- function(train){
  pairs(train[,c(1,3,4,5,8)],col=train$order)
}

## BOXPLOTS ##

get_boxplots <- function(train){
  
  # select all numerical variables except certain ones
  # which ones are numerical?
  numericals<-names(sapply(train, is.numeric))
  remove <- c("adFlag","availability","cPriceNA","unit","genericProduct","salesIndex","campaignIndex","X3.1.1.Unique_pids_per_group_binned.DB","X3.1.2.Unique_pids_per_group_binned_2.DB","X3.2.1.Unique_pids_per_manufacturer_binned.DB","X3.2.2.Unique_pids_per_manufacturer_binned_2.DB","X3.3.1.Unique_pids_per_category_binned.DB","X3.4.1.Unique_pids_per_day_binned.DB","day2")
  tobetreated <-numericals [! numericals %in% remove]
  tobetreated <- tobetreated[tobetreated %in% colnames(train)]
  
  # rearrange data (longform)
  train.m <- reshape2::melt(train[,tobetreated], id.vars = "order") # only take numerical values for boxplot
  
  # plot
  p <- ggplot(data = train.m, aes(x=variable, y=value)) 
  p + geom_boxplot(aes(fill="order")) + facet_wrap( ~ variable, scales="free") 
}


###Missing values

missing <- function(data){
missmap(data, main = "Missingness Map Dataset name")
}