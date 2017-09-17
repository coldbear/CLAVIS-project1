#################################################################### 

### Model Evaluation ###

#################################################################### 
# Set the working directory 
# Uncomment these lines to set the working directory to the current file's location
# if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Note : For the rsse decomposition to work the dataset needs to have the price variable
test <- readRDS("test")

# Evaluation metric
sse <- function(actual, pred) {
  error = sum((actual - pred)^2)
  return(error)
}

# Custom results function

return_confmat <- function(rf.model, test, actual,threshold,pos,neg,threshold_name){
  
  probabilities = predict(rf.model, newdata = test,type = "prob")[,2]
  print("Actual")
  print(table(actual))
  
  pred= as.factor(ifelse(probabilities >= threshold, pos, neg))
  print(paste0("Predicted ",threshold))
  print(table(pred))
  
  print("Confusion Matrix")
  
  if(length(levels(pred))>1){
    confmat <- caret::confusionMatrix(actual,pred)
    print(confmat)
    cmplot <- fourfoldplot(confmat$table, color = c("#0000FF","#FF0000"),
                           conf.level = 0, margin = 1,
                           main =paste0("Threshold value - ",threshold," - ",
                                        threshold_name))
  }else{print("classification has only one level"); break}
  return(pred)
}


error_decomposition <- function(rf.model,act,type,threshold,threshold_name){
 
  probabilities = predict(rf.model, newdata = test,type = "prob")[,2]
  
  pred= as.factor(ifelse(probabilities >= threshold, 1, 0))
  
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
                      " value item with threshold value : ",threshold_name," - ",threshold))
  legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
  
  dist.plot
  return(error)
}

