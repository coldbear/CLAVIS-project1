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
create_roccurve <- function(y = "order", model, test){
  
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
  
  
  ################## ROC CURVE #####################
  
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
  #plot_interactive_roc(basicplot) #this is the initial line
  plot(basicplot)
  
  # Optimal Cutoff
  optimalcutoff <- optimalCutoff(actuals = y, predictedScores = yhat, optimiseFor = "Both")
  # AUC
  auc <- round(calc_auc(basicplot)$AUC, 2)
  
  # code for interactive ROC Curve Implementation  
  return(c(paste("OptimalCutoff:",optimalcutoff), paste("AUC:", auc)))
  
}  

####################################################################################################
####################################################################################################
####################################################################################################


# PRECISION-RECALL CURVE
#create_prcurve("order", model, test)
create_prcurve <- function(y = "order", model, test){
  
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
  
  # plot
  plot(pr.curve(scores.class0 = yhat, weights.class0 = y, curve = TRUE) )
  
  
}  


####################################################################################################
####################################################################################################
####################################################################################################


# UPLIFT CURVES
#create_liftcurve("order", rf.model, test)
create_liftcurve <- function(y = "order", model, test){
  
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
  
  # plot
  ks_plot(actuals=y, predictedScores=yhat)
  
}  





smp_size <- floor(0.7* nrow(data))

saveRDS(test, "test")
saveRDS(train, "train")
## set the seed to make your partition reproductible
set.seed(123)
idx <- createDataPartition(data$order,p = 0.7, list = FALSE)


train <- data[idx, ]
test <- data[-idx, ]
x_train <- train[,-2]
y_train <- train[,2]

rf.model <- randomForest(order~., data=train, ntree=1000, mtry=c(6,8,10), importance=TRUE)
rf.model
saveRDS(rf.model, "rf.model")




server <- function(input, output) {
  output$plot <- renderPlot({
    input$goPlot # Re-run when button is clicked
    
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    plot(dat$x, dat$y)
  })
}

ui <- shinyUI(basicPage(
  plotOutput('plot', width = "300px", height = "300px"),
  actionButton('goPlot', 'Go plot')
))

shinyApp(ui = ui, server = server)


