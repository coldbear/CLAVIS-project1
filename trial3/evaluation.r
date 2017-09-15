#################################################################### 

### Model Evaluation ###

#################################################################### 
# Set the working directory 
# Uncomment these lines to set the working directory to the current file's location
# if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Evaluation metric
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
  }
  #else{print("classification has only one level"); break}
  
  #Calculate rsse
  revenue.actual <- test$price*(as.numeric(test$order)-1)
  print(summary(revenue.actual))
  
  revenue.pred <- (as.numeric(pred)-1)*test$price
  print(summary(revenue.pred))
  
  error <- user(revenue.actual,revenue.pred)
  print(paste0("The RSSE from prediction using threshold value ", threshold))
  print(error)
  
  # Error decomposition
  mse = mean((revenue.actual - revenue.pred)^2)
  bias = (mean(revenue.pred) - mean(revenue.actual))
  var = mse - (bias^2) #(Decomposition of mse)
  percent.var = var/mse
  actual.skew = skewness(revenue.actual)  # to check the distributions
  predicted.skew = skewness(revenue.pred)
  
  # Saving results
  
  error.matrix = cbind(error, mse, bias,var,percent.var,actual.skew, predicted.skew)
  cat("\n", "Decomposing the error : ", "\n")
  print(error.matrix)
  
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
  
  # pred = as.vector(pred)
  # return(pred)
}

# Evaluation function 
#rf.model <- readRDS("rfe.train.RDS") - use an rf model specific to the train set, 
# dataset used for training and testing should match 
#                  modelname = "Random Forest",
#                data = test,actual = test$order,
#               pos = 1,neg = 0,
#           threshold="Mean")


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



#rf.model <- readRDS("rfe.train.RDS") - use an rf model specific to the train set, 
# dataset used for training and testing should match 
#                  modelname = "Random Forest",
  #                data = test,actual = test$order,
   #               pos = 1,neg = 0,
       #           threshold="Mean")
#
