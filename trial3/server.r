
############################################################
'CLAVIS APP'
############################################################




#Libraries
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("ICEbox")) install.packages("ICEbox"); library("ICEbox")
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("utils")) install.packages("caret"); library("utils")
if(!require("Boruta")) install.packages("Boruta"); library("Boruta")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("Amelia")) install.packages("Amelia"); library("Amelia")
if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
if(!require("InformationValue")) install.packages("InformationValue"); library("InformationValue")
if(!require("PRROC")) install.packages("PRROC"); library("PRROC")



#setwd("/Users/coldbear/Desktop/APAPaper/APA-project/trial3")

#Load data 
data <- readRDS("workset")
rf.model <- readRDS("rf.model")
test <- readRDS("test")
train <- readRDS("train")



#Load  functionality
source("dataexploration.r")
source("modelselection.r")
source("varselection.r")
source("evaluation.r")
source("interpretation.r")



#Start of server function

server <- function(input, output) {
  
  
           #####DATA  EXPLORATION TAB#####
# Load data observations
  output$datatable <- renderTable({
  head(data, n = input$obs)
})
  
# Generate a summary of the preloaded dataset
  
output$summarydata <- renderPrint({
    summary(data)
  })
# Generate a summary of the preloaded model
output$summarymodel <- renderPrint({
  rf.model
})

#Graphs

output$exploration1 <- renderPlot({
  explora1(orignal, train)
})

output$exploration2 <- renderPlot({
  explora2(train)
})

           #####MODEL SELECTION TAB#####
output$cutoffpoint <- renderPrint({
  create_roccurve("order", rf.model, test)
})
output$roccurve <- renderPlot({
  create_roccurve("order", rf.model, test)
})
output$prcurve <- renderPlot({
  create_prcurve("order", rf.model, test)
})
output$liftcurve <- renderPlot({
  create_liftcurve("order", rf.model, test)
})


         #####VARIABLE SELECTION TAB#####

#Load the variable selection plotsplots

output$vs <- renderPlot({
  Select_Features(input$method)
})



            #####EVALUATION TAB#####

output$eval <- renderPlot({
  evaluate(rf.model,'Random Forest',test, test$order, 1,0,threshold=input$evalthresh)
  #evaluate(rf.model, modelname = "Random Forest",data = test,actual = test$order, pos = 1,neg = 0,threshold="Mean")
})

output$evaloutput <- renderPrint({
  capture.output(evaluate(model = rf.model,
                          modelname = "Random Forest",
                          data = test,actual = test$order,
                          pos = 1,neg = 0,
                          threshold=input$evalthresh))
})


            #####INTERPRETATION TAB#####

  output$varimp <- renderPlot({
      
    varimp(rf.model)
  })

  output$pdp <- renderPlot({
    pdp (rf.model,data)
  })
  output$ice <- renderPlot({
    ice(input$icevar)
  })
}
