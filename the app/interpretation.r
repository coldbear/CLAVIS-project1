#this function returns  an  rf model
#bbbb <- rf(fold, 0.4,"order", 400, 10)
rf <- function(data, train_size, var_clus, rf_ntree, rf_mtry ) {
  
 #Preparing the partition index
   train_rows <- function(data, train_size){
    x <- createDataPartition(data, p = train_size, list = FALSE)
    return(x)
  }
  set.seed(1000)
    
    # Training & Test Set Data 
    train_rows <- train_rows(data = data[, var_clus], train_size = train_size)
    y_train <- data[train_rows, var_clus]
    x_train <- data[train_rows, -which(names(data) %in% var_clus)] 
    y_test <- data[-train_rows, var_clus]
    x_test <- data[-train_rows, -which(names(data) %in% var_clus)] 
    
    # Random forest in normal mode
    rf_model_normal <- randomForest(x = x_train, y = y_train, xtest = x_test, 
                                    ytest = y_test, ntree = rf_ntree, mtry = rf_mtry, 
                                    importance = TRUE, proximity = TRUE, keep.forest = TRUE)
    # Adding data to the model
    ln <- length(rf_model_normal)
    rf_model_normal[[ln+1]] <- data
    rf_model_normal[[ln+2]] <- data[, -which(names(data) %in% var_clus)]
    names(rf_model_normal)[(ln+1):(ln+2)] <- c("org_data", "org_data_wo_class")
    
    return(rf_model_normal) # Return random forest object
  }



#This function  gives a varImpPlot
#varimp_pd(fold, 0.4,"order", 400, 10)
varimp_pd <- function(data, train_size, var_clus, rf_ntree, rf_mtry ) {
  random_forest <- rf(data, train_size, var_clus, rf_ntree, rf_mtry)
  varImpPlot(random_forest, type = 2)
 # importanceOrder=order(-random_forest$importance)
 # names <- rownames((random_forest$importance))[importanceOrder][1:55]
 # names <- as.factor(names)
 # list <- as.list(levels(names))
 # list <- unlist(list)
 # par(mfrow=c(2,2))
 # partialPlot(random_forest, data, pid)
  #partialPlot(random_forest, data, X15.1.adFlag_last_1_day.Mat)
  #partialPlot(random_forest, data, X15.3.adFlag_last_1_obs.Mat)
  #partialPlot(random_forest, data, X15.5.adFlag_last_1_obs_norm.Mat)
  #partialPlot(random_forest, data, day2)
  #partialPlot(random_forest, var_clus, data[,list[1]])
  #partialPlot(random_forest, var_clus, list[2])
  #partialPlot(random_forest, var_clus, list[3])
  #partialPlot(random_forest, var_clus, list[4])
}

