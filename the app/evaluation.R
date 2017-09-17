#################################################################### 

### Model Evaluation ###

#################################################################### 
# Set the working directory 
# Uncomment these lines to set the working directory to the current file's location
# if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Note : For the rsse decomposition to work the dataset needs to have the price variable


# Evaluation metric
sse <- function(actual, pred) {
  error = sum((actual - pred)^2)
  return(error)
}


# Custom results function
return_confmat <- function(model,actual,threshold,pos,neg){
  
  probabilities = predict(rf.model, newdata = test,type = "prob")[,2]
  stats = summary(probabilities)
  
  if(threshold == "Random") {
    t_value = 0.5
    t_name = "Random"}
  
  if(threshold == "Mean") {
    t_value = stats[4]
    t_name = "Mean"}
  
  if(threshold == "Median") {
    t_value = stats[3]
    t_name = "Median"}
  
  if(threshold == "3quad") {
    t_value = stats[5]
    t_name = "3 quad"}
  
  if(is.numeric(threshold)){
    t_value = as.numeric(threshold)
    t_name = "User"
  }
  
  
  pred = as.factor(ifelse(probabilities >= t_value, pos, neg))
  
  print("Confusion Matrix")
  
  if(length(levels(pred))>1){
    confmat <- caret::confusionMatrix(actual,pred)
    print(confmat)
    cmplot <- fourfoldplot(confmat$table, color = c("#0000FF","#FF0000"),
                           conf.level = 0, margin = 1,
                           main =paste0("Threshold value - ",t_value," - ",
                                        t_name))
  }else{print("classification has only one level"); break}
  return(pred)
}


error_decomposition <- function(model,actual,type,threshold){
  
  
  probabilities = predict(rf.model, newdata = test,type = "prob")[,2]
  stats = summary(probabilities)
  pred = return_confmat(rf.model,test$order,threshold,1,0)
  
  if(threshold == "Random") {
    t_value = 0.5
    t_name = "Random"}
  
  if(threshold == "Mean") {
    t_value = stats[4]
    t_name = "Mean"}
  
  if(threshold == "Median") {
    t_value = stats[3]
    t_name = "Median"}
  
  if(threshold == "3quad") {
    t_value = stats[5]
    t_name = "3 quad"}
  
  if(is.numeric(threshold)){
    t_value = as.numeric(threshold)
    t_name = "User"
  }
  
  
  # Calculate revenue
  revenue.actual <- test$price*(as.numeric(test$order)-1)
  revenue.pred <- (as.numeric(pred)-1)*test$price
  
  #Calculate rsse
  sse.full <- sse(revenue.actual,revenue.pred)
  error <- sqrt(sse.full)
  print(paste0("The RSSE from prediction is"))
  cat("\n",error)
  
  #High-low split
  res = as.data.frame(cbind(revenue.actual,revenue.pred))
  res.low <- subset(res,res$revenue.actual<4)
  res.high <- subset(res,res$revenue.actual>=4)
  
  # RSSE decomposition
  if(type=="low"){
    actual <- res.low$revenue.actual
    predicted <- res.low$revenue.pred
  }else{
    actual <- res.high$revenue.actual
    predicted <- res.high$revenue.pred
  }
  sse<- sse(actual,predicted)
  percent.error <- sse/sse.full
  cat("\n","The percentage of error for " ,type ,"value items is ",percent.error)
  
  bias = (mean(actual) - mean(predicted))
  actual.skew = skewness(actual)  # to check the distributions
  predicted.skew = skewness(predicted)
  
  error= cbind(sse,percent.error,bias,actual.skew, predicted.skew)
  cat("\n", "Decomposing the error for",type,"value items: ", "\n")
  print(error)
  
  # Visualization - Plotting the normal curve - low value 
  x1 = actual
  y1= dnorm(actual,mean = mean(actual), sd = sd(actual))
  
  x2 = predicted
  y2 = dnorm(predicted,mean = mean(predicted), sd = sd(predicted))
  
  cat("\n", "Plotting the distributions of the actual and predicted values")
  
  dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)),
                   ylim = range(c(y1,y2)), xlab = "Revenue", ylab = "Density")
  points(x2, y2, col = "red")
  title(main = paste0("Distribution of actual vs predicted for ", type ,"\n ",
                      " value item with threshold value : ",t_name,"-",t_value))
  legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
  
  dist.plot
  return(error)
}



#Evaluate the predictions 
#use an rf model specific to the train set,dataset used for training and testing should match 
#rf.model <- readRDS("rf.model.ji.RDS")
#test <- readRDS("test")
#rf.model <- randomForest(order~.,train,ntree=500,mtry=8)

############
# Function call for confusion matrix 

#pred = return_confmat(rf.model,test$order,"Mean",1,0)

###################
# Function call for error decomposition
#low.decompose <- error_decomposition(act=test$order,
#                                 pred= pred,
#                             "low","Mean")

#high.decompose <- error_decomposition(act=test$order,
#                            pred= pred,
#                            "high","Mean")


