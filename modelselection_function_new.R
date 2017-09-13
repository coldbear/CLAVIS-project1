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

modelselectionfunction <- function(y = "order", model, test){
  
  # load package
  if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
  if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  
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
  yhat <- predict(model, newdata = test, type = "prob")[,2]
  
  # combine predictions & true value in a data frame
  data = data.frame(y, yhat)
  
  
  
  ####################       ROC CURVES       ####################
  # plot

    # define style
    basicplot <- ggplot(data, aes(d = y, m = yhat)) + 
      geom_roc(labelround = 2, 
               cutoffs.at = NULL, 
               cutoff.labels = NULL,
               show.legend = NA, 
               inherit.aes = TRUE,
               color = "#FF99FF") +
      style_roc(xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")
    # output interactive plot
    plot_interactive_roc(basicplot)
    # output code
    code <-  export_interactive_roc(basicplot)

  
  optimalcutoff <- optimalCutoff(actuals = y, predictedScores = yhat, optimiseFor = "Both")
  
  
  
  
  ####################       PRECISION-RECALL CURVES       ####################
  
    pr <- pr.curve(scores.class0 = yhat, weights.class0 = y, curve = TRUE) 

  
  
  ####################       UPLIFT CURVES       ####################
  
    ks_plot_i <- ks_plot(actuals=y, predictedScores=yhat)

  
  
  # code for interactive ROC Curve Implementation  
  return(c(code, paste("OptimalCutoff:",optimalcutoff)))
  
  }





