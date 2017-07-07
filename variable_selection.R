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
idx = createDataPartition(y = fold_1$order, p = 0.034, list = FALSE)
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
selected_features = c("pid","content","group","category","X10.meanPrice_deviation_percentage.Mat","X12.lastObservation.Mat","X13.1.adFlag_prop_obs_5.Mat",	"X13.2.adFlag_prop_obs_10.Mat",	"X13.3.adFlag_prop_obs_15.Mat",	"X13.4.adFlag_prop_obs_20.Mat",	"X13.5.adFlag_prop_obs_30.Mat",	"X13.6.adFlag_prop_obs_50.Mat",	"X15.1.adFlag_last_1_day.Mat",	"X15.2.adFlag_last_0_day.Mat",	"X15.3.adFlag_last_1_obs.Mat",	"X15.4.adFlag_last_0_obs.Mat",	"X15.5.adFlag_last_1_obs_norm.Mat",	"X20.3.2.1availability2_prop_obs_15.Mat",	"X20.7.3.availability3_prop_obs_75.Mat",	"X20.8.3.availability3_prop_obs_100.Mat",	"X20.8.4.availability4_prop_obs_100.Mat",	"X25.pid_prop_per_day.Mat",	"X3.3.Unique_pids_per_category.DB",	"X3.4.Unique_pids_per_day.DB",	"X7.weekday.Mat",	"19.1.1.availability_changes12_obs_5.Mat",	"19.1.11.availability_changes42_obs_5.Mat",	"19.2.2.availability_changes13_obs_10.Mat",	"19.2.4.availability_changes21_obs_10.Mat",	"19.4.8.availability_changes32_obs_20.Mat",	"19.5.11.availability_changes42_obs_30.Mat",	"19.6.5.availability_changes23_obs_50.Mat",	"19.6.6.availability_changes24_obs_50.Mat",	"19.6.7.availability_changes31_obs_50.Mat",	"19.7.8.availability_changes32_obs_75.Mat",	"19.8.3.availability_changes14_obs_100.Mat",	"19.8.4.availability_changes21_obs_100.Mat",	"19.8.9.availability_changes34_obs_100.Mat",	"cPriceNA",	"unit",	"genericProduct",	"salesIndex",	"campaignIndex","X3.1.1.Unique_pids_per_group_binned.DB","X3.1.2.Unique_pids_per_group_binned_2.DB","X3.2.1.Unique_pids_per_manufacturer_binned.DB","X3.2.2.Unique_pids_per_manufacturer_binned_2.DB","X3.3.1.Unique_pids_per_category_binned.DB","X3.4.1.Unique_pids_per_day_binned.DB","day2")
x = x[,selected_features]

#select numeric variables and standardize them
tobetreated = selectforoutlier(x)
x[,tobetreated] = sapply(x[,tobetreated], normalize)

#create dummies for factors and include all variables in the set of covariates
x = model.matrix(~.-1,data = x, contrasts.arg = lapply(x[,sapply(x, is.factor)], contrasts, contrasts = FALSE))

#LASSO
fit = glmnet(x = x, y = fold_1$order)
plot(fit)
cvfit = cv.glmnet(x = x, y = fold_1$order)
plot(cvfit)
coef(cvfit, s = "lambda.min")[which(coef(cvfit, s = "lambda.1se") != 0)]




print_glmnet_coefs <- function(cvfit, s="lambda.1se") {
  ind <- which(coef(cvfit, s=s) != 0)
  df <- data.frame(
    feature=rownames(coef(cvfit, s=s))[ind],
    coeficient=coef(cvfit, s=s)[ind]
  )
  kable(df)
}

print_glmnet_coefs(cvfit = cvfit)




