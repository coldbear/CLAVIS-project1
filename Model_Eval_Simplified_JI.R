#################################################################### 

### Model Evaluation ###

#################################################################### 
# Set the working directory 
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(caret)

# Evaluation metric
rmse = function(actual, pred) {
  error = sqrt(mean((actual - pred)^2))
  return(error)
}

user <- function(actual, pred) {
  rsse = sqrt(sum((actual - pred)^2))
  return(rsse)
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
    confmat<- confusionMatrix(actual,pred)
    print(confmat)
    cmplot <- fourfoldplot(confmat$table, color = c("#0000FF","#FF0000"),
                                  conf.level = 0, margin = 1,main =paste0("Threshold value - ",name) )
  }else{print("classification has only one level"); break}
  
  # Calculate rsse
  revenue.actual <- test$price*(as.numeric(test$order)-1)
  print(summary(revenue.actual))
  
  revenue.pred <- (as.numeric(pred)-1)*test$price
  print(summary(revenue.pred))
  
  error <- user(revenue.actual,revenue.pred)
  print(paste0("The RSSE from prediction using threshold value ", threshold))
  print(error)
  
  # Visualization - Plotting the normal curve
  x1 = revenue.actual
  y1 = dnorm(revenue.actual, mean = mean(revenue.actual), sd = sd(revenue.actual))
  
  x2 = revenue.pred
  y2 = dnorm(revenue.pred, mean = mean(revenue.pred), sd = sd(revenue.pred))
  
  cat("\n", "Plotting the distributions of the actual and predicted values - green actual,red predicted")
  
  dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)),
                   ylim = range(c(y1,y2)), xlab = "Revenue", ylab = "Density")
  points(x2, y2, col = "red")
  title(main = paste0("Threshold value : ",threshold))
  legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))

  dist.plot
  
  pred = as.vector(pred)
  return(pred)
}

# Evaluation function 

evaluate = function(type,model,modelname,data,actual,pos,neg,threshold,metric) {
  
  #Loading packages
  if (!require("e1071")) install.packages("e1071"); library("e1071")
  if (!require("randomForest")) install.packages("randomForest"); library("randomForest")

  if (type == "Classification") {
  
    probabilities = predict(model, newdata = data)
    stats = summary(probabilities)
    print("Descriptive statistics for the prediction probabilities : ")
    print(stats)

    print("Classification using 0.5,mean of the probabilities as thresholds")
    
    # Function call to results function
    
    pred.mean <- results(probabilities,actual,stats[3],pos,neg,"Mean")
    pred.median <- results(probabilities,actual,stats[4],pos,neg,"Median")
    pred.3quad <- results(probabilities,actual,stats[5],pos,neg,"Third Quadrant")
    pred.random <- results(probabilities,actual,0.5,pos,neg,"Random")
    
    if(!is.null(threshold)){
      print(paste0("Using user defined threshold - ",threshold))
      pred.user <- results(probabilities,actual,threshold,pos,neg,"User Defined") 
    }
    
  } else {
    if (type == "Regression") {
      
      # Predictions
      pred = predict(model, data)
      pred = ifelse(pred <= 0, 0, pred)  ## Removing negative values
      
      cat("\n", "The descriptive statistics of the predicted values", "\n")
      print(summary(pred))
      cat("\n", "The descriptive statistics of the actual values", "\n")
      print(summary(actual))
      
      if(metric!="rmse"&metric!="user"){
        stop("Choose an appropriate method. Check for typos!")}
      
      if (metric == "rmse") {
        
        error = rmse(actual, pred)
        cat(paste0("\n", "The prediction error is : ", error))
      } else {
        error = user(actual, pred)
      }
      
      # Error decomposition
      mse = mean((actual - pred)^2)
      bias = (mean(pred) - mean(actual))
      var = mse - (bias^2) #(Decomposition of mse)
      percent.var = var/mse
      actual.skew = skewness(actual)  # to check the distributions
      predicted.skew = skewness(pred)
      
      # Saving results
      
      error.matrix = cbind(error, mse, bias,var,percent.var,actual.skew, predicted.skew)
      cat("\n", "Decomposing the error : ", "\n")
      print(error.matrix)
      
      # Visualization - Plotting the normal curve
      
      x1 = actual
      y1 = dnorm(actual, mean = mean(actual), sd = sd(actual))
      
      x2 = pred
      y2 = dnorm(pred, mean = mean(pred), sd = sd(pred))
      
      cat("\n", "Plotting the distributions of the actual and predicted values - green actual,red predicted")
      
      dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)), ylim = range(c(y1, 
                                                                                      y2)), xlab = "Sales", ylab = "Density")
      points(x2, y2, col = "red")
      title(main = as.character(modelname))
      legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
      
      dist.plot
      
      pred = as.vector(round(pred))
      return(pred)}
  }
  
}


#Evaluate the predictions - (Add your responses in the console)
lr.model <- lm(train$order~.,train)
rf.model <- readRDS("rfe.train.RDS")

lr.res = evaluate(type = "Classification",
                  model = lr.model,
                  modelname = "Linear Regression",
                  data = test,actual = test$order,
                  pos = "yes",neg = "no",
                  threshold=NULL,metric = NULL)

rf.res = evaluate(type = "Classification",
                  model = rf.model,
                  modelname = "Random Forest",
                  data = test,actual = test$order,
                  pos = "yes",neg = "no",
                  threshold=NULL,metric = NULL)


