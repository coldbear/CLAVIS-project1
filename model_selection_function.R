### MODEL SELECTION ###




#############################################################################
#
#            MODEL SELECTION: ROC CURVES; PR CURVES; UPLIFT CURVES
#
#############################################################################

# INPUT
# y = vector of true values
# yhat = vector with predictions or data.frame with more than one vector of predictions
# if double = TRUE for more than one set of predictions, they are displayed in separate graphs

# OUTPUT
# interactive roc curve
# pr curve
# uplift curve
# html/ javascript code (for embedding)

modelselectionfunction <- function(y, yhat,  
  labelround = 2, 
  cutoffs.at = NULL, 
  cutoff.labels = NULL,
  show.legend = NA, 
  inherit.aes = TRUE,
  color = "#FF99FF",
  double= FALSE){
  
  # load package
  if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
  if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  
  # check yhat
  if(is.vector(yhat)){
    ncol = 1
    yhat = as.data.frame(yhat)
  } else if(is.data.frame(yhat)){
    ncol = ncol(yhat)
  } else {
    stop("Input has wrong format. It must either be a vector (for 1 set of prediction) or a 
         data.frame (for more than 1 set of predictions)")
  }
  
  # check length of y, yhat
  if(length(y) != nrow(yhat)){
      stop("y, yhat must have same length")
  }
  
  # check y is numeric with values {0,1}
  if(!is.numeric(y)){
    stop("y, yhat must be numeric")
  } 
  if(!(y == 0 || y ==1)){
    stop("y must be either 0 or 1")
  }
  
  # combine predictions & true value in a data frame
  data = data.frame(y, yhat)
  
  
  
  ####################       ROC CURVES       ####################
  # plot
  if(ncol == 1){
    
    # rename column of interet
    colnames(data)[2] <- "M1"
    # define style
    basicplot <- ggplot(data, aes(d = y, m = M1)) + 
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
    
  } else {
    
    # convert data to longform
    data.longform <- melt_roc(data, 1, c(2:(ncol+1)))
    
    if(double == TRUE){
      # define style
      plot <- ggplot(data.longform, aes(d = D, m = M, color = name)) + 
                geom_roc(labelround = 2, 
                cutoffs.at = NULL, 
                cutoff.labels = NULL,
                show.legend = NA, 
                inherit.aes = TRUE) + 
                facet_wrap(~ name) + 
        style_roc(xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")
    } else{
      # define style
      plot <- ggplot(data.longform, aes(d = D, m = M, color = name)) + 
                geom_roc(labelround = 2, 
                cutoffs.at = NULL, 
                cutoff.labels = NULL,
                show.legend = NA, 
                inherit.aes = TRUE) + 
        style_roc(xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")
      }

    # output interactive plot
    plot_interactive_roc(plot)
    # output code
    code <-  export_interactive_roc(plot)
  }  

  for(i in 1:ncol){
    optimalcutoff <- optimalCutoff(actuals = y, predictedScores = yhat[,i], optimiseFor = "Both")
  }
 
  
  
  ####################       PRECISION-RECALL CURVES       ####################
  
  for(i in 1:ncol){
    pr <- pr.curve(scores.class0 = yhat[,i], weights.class0 = y, curve = TRUE) 
    plot(pr)
    }
  
  
  
  ####################       UPLIFT CURVES       ####################
  
  for(i in 1:ncol){
    ks_plot_i <- ks_plot(actuals=y, predictedScores=yhat[,i])
    ks_plot_i
  }
  
  
  
  # code for interactive ROC Curve Implementation  
  return(c(code, paste("OptimalCutoff:",optimalcutoff)))
    
  }
  
  
  
  
  
  