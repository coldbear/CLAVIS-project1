### RANDOM FOREST: forestFloor  (http://forestfloor.dk/)

# prelims
rm(list=ls())
set.seed(1)

# packages
source("helper.apaproject.R")

# get data 
source("DataPrep.R")

# take small amount of data
set.seed(222)
idx = createDataPartition(y = data$order, p = 0.50, list = FALSE)
data_small = data[idx,]

# prep data
data_small$order <- as.factor(data_small$order)
y = data_small$order
X = data_small
X = X[,!names(X)=="order"]

#train default model and the most regularized model with same predictive performance
rf.default = randomForest(X,y,ntree=5000)
rf.robust = randomForest(X,y,sampsize=25,ntree=5000,mtry=4,
                         keep.inbag = T,keep.forest = T)  #need for forestFloor

# plot crossvalidated ROC 
plot(roc(rf.default$votes[,2],y),main="ROC: default black, robust is red")

plot(roc(rf.robust$votes[,2],y),col=2,add = T)

# auc
print(auc(roc(rf.default$votes[,2],y)))
print(auc(roc(rf.robust$votes[,2],y)))

# plot
ff = forestFloor(rf.robust,X,binary_reg = T,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5)
plot(ff,col=Col,plot_GOF = T)

# plot 3d
show3d(ff,c(1,5),5,col=Col,plot_GOF = T)

