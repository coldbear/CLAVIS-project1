### MODEL SELECTION ###




#############################################################################
#
#            MODEL SELECTION: ROC CURVES; PR CURVES; UPLIFT CURVES
#
#############################################################################

# INPUT
# y = name or target variable, default: "order"
# test = test data set, default: test
# model = name of model, default: model


# OUTPUT
# interactive roc curve
# pr curve
# uplift curve
# html/ javascript code (for embedding)


# ROC CURVE

create_plots <- function(y = "order", model, train, test, CBTN = 0, CBFN = -1, CBFP = -1, CBTP = 0, plot){
  
  ################## LOAD PACKAGE ##################### 
  
  if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
  if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
  
  
  ################## PREPARE DATA #####################
  
  # check test dataset
  if(!is.data.frame(test)){
    stop("test must be a data frame")
  }
  if(!y %in% colnames(test)){
    stop("test must contain target variable")
  }
  
  y <- as.numeric(test[,y])-1
  # check if target is numeric with values {0,1}
  if(!is.numeric(y)){
    stop("y must be numeric")
  } 
  if(!(y == 0 || y ==1)){
    stop("y must be either 0 or 1")
  }
  
  # generate predictions
  yhat <- data.frame("pred" = predict(model, newdata = test, type = "prob")[,2])
  
  # combine predictions & true value in a data frame
  data = data.frame("order" = y, "pred"= yhat$pred)
  
  ######################################################
  ################### ROC CURVE ########################
  ######################################################

  if(plot == "ROC-Curve"){
    # define style
    basicplot <- ggplot(data, aes(d = order, m = pred)) + 
      geom_roc(labelround = 2, 
               cutoffs.at = NULL, 
               cutoff.labels = NULL,
               show.legend = NA, 
               inherit.aes = TRUE,
               color = "#FF99FF") +
      style_roc(xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")
    # output interactive plot
    plot_interactive_roc(basicplot)

  
  ################## OPTIMAL CUTOFF #####################  
    
    ####### COST MATRIX #######  
    
    # calculate costs with 0 on diagonals
    CFN = CBFN - CBTP
    CFP = CBFP - CBTN
    
    # build cost-matrix
    cost.matrix <- matrix(c(
      0, CFN,
      CFP, 0),
      2, 2, byrow=TRUE)
    
    # name rows and columns
    colnames(cost.matrix) <- list("false", "true")
    rownames(cost.matrix) <- list("prediction: false", "prediction: true")
    
    
    ####### OPTIMAL CUTOFF ####### 
    
    # MODEL.CONTROL
    # Method: maxKappa
    model.control.optc = control.cutpoints(CFP = -cost.matrix[2,1], CFN = -cost.matrix[1,2], costs.ratio = -cost.matrix[2,1]/-cost.matrix[1,2], weighted.Kappa = TRUE)
   
    # RUN OPTIMAL CUTPOINTS
    oc = optimal.cutpoints(
      X = colnames(yhat), 
      status = "order", 
      tag.healthy = "0", 
      methods = "MCT", 
      data = data, 
      control = model.control.optc)
    
    # SELECT OPTIMAL CUTPOINT (compute average, if oc not unique)
    # define temporary dataframe to store cutoffs 
    df <- data.frame(cutoff = oc$MCT$Global$optimal.cutoff$cutoff)
    # calculate average
    optimalcutoff <- mean(df$cutoff)
   
   
    ####### AUC ####### 
    auc <- round(calc_auc(basicplot)$AUC, 2)
  
    ####### RETURN #######
    return(c(paste("OptimalCutoff:",optimalcutoff), paste("AUC:", auc)))
  
  } else if(plot == "PR-Curve"){
  

  ######################################################
  ########## PRECISION-RECALL CURVE ####################
  ######################################################

  # plot
  pr <- pr.curve(scores.class0 = yhat$pred, weights.class0 = y, curve = TRUE) 
  plot(pr)
  } else if(plot == "Uplift-Curve"){ 
  
  
  ######################################################
  ###############    UPLIFT CURVE   ####################
  ######################################################


  # plot
  ks_plot(actuals=data$order, predictedScores=data$pred)
  }
}
  







