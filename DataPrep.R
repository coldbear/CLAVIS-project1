# APA Magic - Data Preparation
setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/APA/Magicalmodel")


# load data
setwd("/Users/sfinkenwirth/Downloads")
new = readRDS("fold_1_b2.rds")
setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/APA/apaproject")

### select 15 variables ###
columns <- floor(runif(14,1,183))
data <- new[,c(columns)]
data$revenue <- new[,"revenue"]
rm(ls = new)

# create order variable, which is one whenever revenue is larger than zer
data$order <-  0
data$order[data$revenue > 0] <- 1
data$order <- as.factor(data$order)
data$revenue <- NULL

# create partition
set.seed = 222
idx = createDataPartition(y = data$order, p = 0.034, list = FALSE)
data <- data[idx,]

# create partition for train and test
set.seed = 222
idx_train = createDataPartition(y = data$order, p = 0.8, list = FALSE)
train = data[idx_train,]
test = data[-idx_train,]


