
###############################
### SKIP TO LINE:138 ##########
###############################


#create dataset
rm(list=ls())

source("helper_variable_selection.R")

#load dataset
fold_1 = readRDS("fold_1_b2.RDS")

#load packages
#install.packages("caret")
#install.packages("glmnet")
library(caret)
library(glmnet)
library(knitr)

#create order variable
fold_1$order = ifelse(fold_1$revenue > 0,1,0)


#take a stratified sample 
set.seed(222)
#set.seed(211)
idx = createDataPartition(y = fold_1$order, p = 0.034, list = FALSE)
idx_test = createDataPartition(y = fold_1$order, p = 0.0025, list = FALSE)
fold_1 = fold_1[idx,]

#Remove variables not needed
fold_1$revenue = NULL
fold_1$lineID = NULL

#factorize variables
factorize = c("adFlag", "availability", "cPriceNA")
fold_1[,factorize] = lapply(fold_1[,factorize], factor)

#find factors
names(Filter(is.factor, fold_1))#14
#xfactors = model.matrix(order~ adFlag + availability + cPriceNA + unit + genericProduct + salesIndex +
#campaignIndex + X3.1.1.Unique_pids_per_group_binned.DB +X3.1.2.Unique_pids_per_group_binned_2.DB +
#X3.2.1.Unique_pids_per_manufacturer_binned.DB + X3.2.2.Unique_pids_per_manufacturer_binned_2.DB + 
#X3.3.1.Unique_pids_per_category_binned.DB + X3.4.1.Unique_pids_per_day_binned.DB + day2, data = fold_1) [-1]

#xfactors = as.data.frame(xfactors)


names(Filter(is.numeric, fold_1)) #168
names(Filter(is.na, fold_1))#0
names(Filter(is.character, fold_1)) #0

#create a df with all covariates
x = fold_1[,-182]


#remove variables that only take value = 0

x$'19.5.12.availability_changes43_obs_30.Mat' = NULL
x$'19.4.12.availability_changes43_obs_20.Mat'= NULL
x$'19.4.3.availability_changes14_obs_20.Mat'= NULL
x$'19.3.12.availability_changes43_obs_15.Mat'= NULL
x$'19.3.3.availability_changes14_obs_15.Mat'= NULL
x$'19.2.12.availability_changes43_obs_10.Mat'= NULL
x$'19.2.6.availability_changes24_obs_10.Mat'= NULL
x$'19.2.3.availability_changes14_obs_10.Mat'= NULL
x$'19.1.12.availability_changes43_obs_5.Mat'= NULL
x$'19.1.6.availability_changes24_obs_5.Mat'= NULL
x$'19.1.3.availability_changes14_obs_5.Mat'= NULL

#factors = c("adFlag","availability","cPriceNA","unit","genericProduct","salesIndex","campaignIndex","X3.1.1.Unique_pids_per_group_binned.DB","X3.1.2.Unique_pids_per_group_binned_2.DB","X3.2.1.Unique_pids_per_manufacturer_binned.DB","X3.2.2.Unique_pids_per_manufacturer_binned_2.DB","X3.3.1.Unique_pids_per_category_binned.DB","X3.4.1.Unique_pids_per_day_binned.DB","day2")
#x = x[,factor_list]
#x = as.matrix(x, xfactors)

#first round selected features
selected_features = c("pid","price","content","group","category","X10.meanPrice_deviation_percentage.Mat","X12.lastObservation.Mat","X13.1.adFlag_prop_obs_5.Mat",	"X13.2.adFlag_prop_obs_10.Mat",	"X13.3.adFlag_prop_obs_15.Mat",	"X13.4.adFlag_prop_obs_20.Mat",	"X13.5.adFlag_prop_obs_30.Mat",	"X13.6.adFlag_prop_obs_50.Mat",	"X15.1.adFlag_last_1_day.Mat",	"X15.2.adFlag_last_0_day.Mat",	"X15.3.adFlag_last_1_obs.Mat",	"X15.4.adFlag_last_0_obs.Mat",	"X15.5.adFlag_last_1_obs_norm.Mat",	"X20.3.2.1availability2_prop_obs_15.Mat",	"X20.7.3.availability3_prop_obs_75.Mat",	"X20.8.3.availability3_prop_obs_100.Mat",	"X20.8.4.availability4_prop_obs_100.Mat",	"X25.pid_prop_per_day.Mat",	"X3.3.Unique_pids_per_category.DB",	"X3.4.Unique_pids_per_day.DB",	"X7.weekday.Mat",	"19.1.1.availability_changes12_obs_5.Mat",	"19.1.11.availability_changes42_obs_5.Mat",	"19.2.2.availability_changes13_obs_10.Mat",	"19.2.4.availability_changes21_obs_10.Mat",	"19.4.8.availability_changes32_obs_20.Mat",	"19.5.11.availability_changes42_obs_30.Mat",	"19.6.5.availability_changes23_obs_50.Mat",	"19.6.6.availability_changes24_obs_50.Mat",	"19.6.7.availability_changes31_obs_50.Mat",	"19.7.8.availability_changes32_obs_75.Mat",	"19.8.3.availability_changes14_obs_100.Mat",	"19.8.4.availability_changes21_obs_100.Mat",	"19.8.9.availability_changes34_obs_100.Mat",	"cPriceNA",	"unit",	"genericProduct",	"salesIndex",	"campaignIndex","X3.1.1.Unique_pids_per_group_binned.DB","X3.1.2.Unique_pids_per_group_binned_2.DB","X3.2.1.Unique_pids_per_manufacturer_binned.DB","X3.2.2.Unique_pids_per_manufacturer_binned_2.DB","X3.3.1.Unique_pids_per_category_binned.DB","X3.4.1.Unique_pids_per_day_binned.DB","day2")
x = x[,selected_features]

#select numeric variables and standardize them
tobetreated = selectforoutlier(x)
x[,tobetreated] = sapply(x[,tobetreated], normalize)


#final dataset
traindata = cbind(x, fold_1$order)
colnames(traindata)[52] = "order"
rm(fold_1)
rm(x)
#test = cbind(x, fold_1$order)

new_names = c("pid",	"price",	"content",	"group",	
              "category",	"meanPrice_deviation",	"last_observation",	
              "adFlag_prop_obs_5",	"adFlag_prop_obs_10",	
              "adFlag_prop_obs_15",	"adFlag_prop_obs_20",	
              "adFlag_prop_obs_30",	"adFlag_prop_obs_50",	
              "adFlag_last_1_day",	"adFlag_last_0_day",	
              "adFlag_last_1_obs",	"adFlag_last_0_obs",	
              "adFlag_last_1_obs_norm",	"availability2_prop_obs_15",	
              "availability3_prop_obs_75",	"availability3_prop_obs_100",	
              "availability4_prop_obs_100",	"pid_prop_per_day",	
              "Unique_pids_per_category",	"Unique_pids_per_day",	
              "weekday",	"availability_changes12_obs_5",	
              "availability_changes42_obs_5",	"availability_changes13_obs_10"
              ,"availability_changes21_obs_10","availability_changes32_obs_20",	
              "availability_changes42_obs_30",	"availability_changes23_obs_50",
              "availability_changes24_obs_50",	"availability_changes31_obs_50",
              "availability_changes32_obs_75",	"availability_changes14_obs_100",
              "availability_changes21_obs_100",	"availability_changes34_obs_100",	
              "cPriceNA",	"unit",	"genericProduct",	"salesIndex",	"campaignIndex",
              "Unique_pids_per_group_binned",	"Unique_pids_per_group_binned_2",	
              "Unique_pids_per_manufacturer_binned",	"Unique_pids_per_manufacturer_binned_2",	
              "Unique_pids_per_category_binned","Unique_pids_per_day_binned",	
              "day2", "order")


colnames(traindata)[1:52] = new_names
colnames(test)[1:52] = new_names

saveRDS(object = traindata, file = "train.rds")



set.seed(223)
idx_2 = createDataPartition(y = fold_1$order, p = 0.7, list = FALSE)
train = traindata[idx_2,]
test = traindata[-idx_2,]
rf.model = readRDS(file = "rfe.train.RDS")
rf.model = lm(formula = order~.,data = train)



## Feature Selection/Importance with RF
#install.packages("randomForest")
library(randomForest)

set.seed(123)
control = rfeControl(functions=rfFuncs, method="cv", number=10)

rfe.train = rfe(traindata[1:51], traindata[,52], sizes=1:12, rfeControl=control)
plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11)
predictors(rfe.train)



## Feature Selection/Importance with Boruata

boruta.train = Boruta(order~., data = traindata, doTrace = 2)
print(boruta.train)


plot(boruta.train, xlab = "", xaxt = "n")
lz=lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) = colnames(boruta.train$ImpHistory)
Labels = sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)



final.boruta = TentativeRoughFix(boruta.train)
print(final.boruta)



getSelectedAttributes(final.boruta, withTentative = F)
boruta.df = attStats(final.boruta)
class(boruta.df)
print(boruta.df)





##############################
#### Feature Selection########
##############################
#install.packages("Boruta")
library(caret)
library(Boruta)
library(randomForest)

#load saved datasets (can be found on slack: apa)

#boruta.train = Boruta(order~., data = traindata, doTrace = 2)
boruta.train = readRDS(file = "boruta.train.RDS")

#library(randomForest)
#set.seed(123)
#control = rfeControl(functions=rfFuncs, method="cv", number=10)
#rfe.train = rfe(traindata[1:51], traindata[,52], sizes=1:12, rfeControl=control)
rfe.train = readRDS(file = "rfe.train.RDS")



Select_Features = function(method = c("Random_Forest", "Boruta_Tentative","Boruta_Final")){ 
  require(Boruta)
  require(randomForest)
  
  if(method == "Random_Forest") {
    #RF Feature Selection
    x=plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:11, Labels = T)
    return(x)
    
  }
  else if(method == "Boruta_Tentative"){
    #Tentative Boruta
    plot(boruta.train,xlab = "", xaxt = "n")
    lz=lapply(1:ncol(boruta.train$ImpHistory),function(i)
      boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
    names(lz) = colnames(boruta.train$ImpHistory)
    Labels = sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
  }
  else if(method == "Boruta_Final"){
    #Final Boruta
    final.boruta = TentativeRoughFix(boruta.train)
    boruta.df = attStats(final.boruta)
    plot(final.boruta, xlab = "", xaxt = "n")
    lz=lapply(1:ncol(final.boruta$ImpHistory),function(i)
      final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
    names(lz) =     names(lz) = colnames(final.boruta$ImpHistory)
    Labels = sort(sapply(lz,median))
    axis(side = 1,las=2,labels = names(Labels),
         at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)
  }
  else {
  "Error: Select a valid method"  
  }
  }


Select_Features(method = "Random_Forest")
Select_Features(method = "Boruta_Tentative")
Select_Features(method = "Boruta_Final")


#Random Forest Feature Importance#
#The plot shows how the RMSE changes as we increase the number of variables,
# the variables corresponding to the lowest RMSE are "selected" and can 
# be found using the "predictors" function.


#TENTATIVE BORUTA EXPLANATION#
#Blue boxplots correspond to minimal, average and maximum Z score of a 
#shadow attribute. Red, yello and green boxplots represent Z scores of
#respectively rejected, tentatively confirmed and confirmed attributes.

## SOURCE: (Kursa and Rudnicki, 2010)

# FINAL BORUTA EXPLANATION#

# Blue boxplots correspond to minimum, average and maximum Z score of a 
# shadow attribute. Red and green boxplots represent Z scores of
# respectively rejected and confirmed attributes#

## SOURCE: (Kursa and Rudnicki, 2010)




