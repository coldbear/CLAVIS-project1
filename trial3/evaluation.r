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
results <- function(probabilities,actual,threshold,pos,neg,name){
  
  print("Actual")
  print(table(actual))
  
  pred= as.factor(ifelse(probabilities >= threshold, pos, neg))
  print(paste0("Predicted ",name))
  print(table(pred))
  
  print("Confusion Matrix")
  
  if(length(levels(pred))>1){
    confmat <- caret::confusionMatrix(actual,pred)
    print(confmat)
    cmplot <- fourfoldplot(confmat$table, color = c("#0000FF","#FF0000"),
                           conf.level = 0, margin = 1,main =paste0("Threshold value - ",name) )
  }else{print("classification has only one level"); break}
  
  #Calculate rsse
  revenue.actual <- test$price*(as.numeric(test$order)-1)
  print(summary(revenue.actual))
  
  revenue.pred <- (as.numeric(pred)-1)*test$price
  print(summary(revenue.pred))
  
  sse.full <- sse(revenue.actual,revenue.pred)
  error <- sqrt(sse.full)
  print(paste0("The RSSE from prediction using threshold value ", threshold))
  cat("\n",error)
  
  # RSSE decomposition
  res = as.data.frame(cbind(revenue.actual,revenue.pred))
  res.low <- subset(res,res$revenue.actual<4)
  res.high <- subset(res,res$revenue.actual>=4)
  
  # Low value item Decomposition
  sse.low <- sse(res.low$revenue.actual,res.low$revenue.pred)
  percent.sse.low <- sse.low/sse.full
  cat("\n","The percentage of error for low value items is ",percent.sse.low)
  
  bias.low = (mean(res.low$revenue.actual) - mean(res.low$revenue.pred))
  actual.skew.low = skewness(res.low$revenue.actual)  # to check the distributions
  predicted.skew.low = skewness(res.low$revenue.pred)
  error.low = cbind(sse.low,bias.low,actual.skew.low, predicted.skew.low)
  cat("\n", "Decomposing the error for low value items: ", "\n")
  print(error.low)
  
  # Visualization - Plotting the normal curve - low value 
  x1.low = res.low$revenue.actual
  y1.low = dnorm(res.low$revenue.actual, 
                 mean = mean(res.low$revenue.actual), sd = sd(res.low$revenue.actual))
  
  x2.low = res.low$revenue.pred
  y2.low = dnorm(res.low$revenue.pred, 
                 mean = mean(res.low$revenue.pred), sd = sd(res.low$revenue.pred))
  
  cat("\n", "Plotting the distributions of the actual and predicted values for low revenue items")
  
  dist.plot.low = plot(x1.low, y1.low, col = "green", xlim = range(c(x1.low, x2.low)),
                       ylim = range(c(y1.low,y2.low)), xlab = "Revenue", ylab = "Density")
  points(x2.low, y2.low, col = "red")
  title(main = paste0("Distribution of actual vs predicted for 
                      low value item with threshold value : ",threshold))
  legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
  
  dist.plot.low
  
  # High value item Decomposition
  sse.high <- sse(res.high$revenue.actual,res.high$revenue.pred)
  percent.sse.high <- sse.high/sse.full
  cat("\n","The percentage of error for high value items is ",percent.sse.high)
  
  bias.high = (mean(res.high$revenue.actual) - mean(res.high$revenue.pred))
  actual.skew.high = skewness(res.high$revenue.actual)  # to check the distributions
  predicted.skew.high = skewness(res.high$revenue.pred)
  error.high = cbind(sse.high,bias.high,actual.skew.high, predicted.skew.high)
  cat("\n", "Decomposing the error for high value items: ", "\n")
  print(error.high)
  
  # Visualization - Plotting the normal curve - high value 
  x1.high = res.high$revenue.actual
  y1.high = dnorm(res.high$revenue.actual, 
                  mean = mean(res.high$revenue.actual), sd = sd(res.high$revenue.actual))
  
  x2.high = res.high$revenue.pred
  y2.high = dnorm(res.high$revenue.pred, 
                  mean = mean(res.high$revenue.pred), sd = sd(res.high$revenue.pred))
  
  cat("\n", "Plotting the distributions of the actual and predicted values for high revenue items")
  
  dist.plot.high = plot(x1.high, y1.high, col = "green", xlim = range(c(x1.high, x2.high)),
                        ylim = range(c(y1.high,y2.high)), xlab = "Revenue", ylab = "Density")
  points(x2.high, y2.high, col = "red")
  title(main = paste0("Distribution of actual vs predicted for 
                      high value item with threshold value : ",threshold))
  legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
  
  dist.plot.high
  
  
}

# Evaluation function 

evaluate = function(model,modelname,data,actual,pos,neg,threshold) {
  
  #Loading packages
  if (!require("e1071")) install.packages("e1071"); library("e1071")
  if (!require("randomForest")) install.packages("randomForest"); library("randomForest")
  
  probabilities = predict(model, newdata = data,type = "prob")[,2]
  stats = summary(probabilities)
  print("Descriptive statistics for the prediction probabilities : ")
  print(stats)
  
  print("Classification using 0.5,mean of the probabilities as thresholds")
  
  # Function call to results function
  if(threshold=="Random"){
    print(paste0("Using threshold - ",threshold))
    pred.random <- results(probabilities,actual,0.5,pos,neg,"Random")
  }
  if(threshold=="Mean"){
    print(paste0("Using threshold - ",threshold))
    pred.mean <- results(probabilities,actual,stats[3],pos,neg,"Mean")
  }
  if(threshold=="Median"){
    print(paste0("Using threshold - ",threshold))
    pred.median <- results(probabilities,actual,stats[4],pos,neg,"Median")
  }
  if(threshold=="3quad"){
    print(paste0("Using threshold - ",threshold))
    pred.3quad <- results(probabilities,actual,stats[5],pos,neg,"Third Quadrant")
  }
  if(is.numeric(threshold)){
    print(paste0("Using user defined threshold - ",threshold))
    pred.user <- results(probabilities,actual,threshold,pos,neg,"User Defined") 
  }
}

