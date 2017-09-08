### MODEL SELECTION ###




#############################################################################
#
#                                 ROC CURVES
#
#############################################################################

# INPUT
# y = vector of true values
# yhat = vector with predictions or data.frame with more than one vector of predictions
# if double = TRUE for more than one set of predictions, they are displayed in separate graphs

# OUTPUT
# interactive roc curve
# html/ javascript code (for embedding)

plot_roccurves <- function(y, yhat,  
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
  
  # plot
  if(ncol == 1){
    
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
    
  } else {
    
    # convert data to longform
    data.longform <- melt_roc(data, data[,1], data[,-1])
    
    if(double == TRUE){
      # define style
      plot <- ggplot(data, aes(aes(d = D, m = M, color = name))) + 
        geom_roc(labelround = 2, 
                 cutoffs.at = NULL, 
                 cutoff.labels = NULL,
                 show.legend = NA, 
                 inherit.aes = TRUE) + 
        facet_wrap(~ name) + 
        style_roc(xlab = "False Positive Rate (1 - Specificity)", ylab = "True Positive Rate (Sensitivity)")
    } else{
      # define style
      plot <- ggplot(data, aes(d = D, m = M, color = name)) + 
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
    
  return(code)  
    
  }
  
  
  
  
  
  