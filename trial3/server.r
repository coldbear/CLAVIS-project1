
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
if(!require("reshape2")) install.packages("reshape2"); library("reshape2")
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

output$boxplots <- renderPlot({
  get_boxplots(train)
})

           #####MODEL SELECTION TAB#####
output$modelselecttext <- renderPrint({
  create_plots("order", rf.model, train, test,CBTN = input$CBTN, CBFN = input$CBFN, CBFP = input$CBFP, CBTP = input$CBTP, input$plottype)
})
output$modelselect <- renderPlot({
  create_plots("order", rf.model, train, test,CBTN = input$CBTN, CBFN = input$CBFN, CBFP = input$CBFP, CBTP = input$CBTP, input$plottype)
})


         #####VARIABLE SELECTION TAB#####

#Load the variable selection plotsplots

output$vs <- renderPlot({
  Select_Features(input$method)
})



            #####EVALUATION TAB#####

output$confmatrix <- renderPlot({
  return_confmat(rf.model, test, test$order, input$evalthreshnum, 1,0, input$evalthresh)
})

#output$confmatrixoutput <- renderPrint({
#  capture.output(return_confmat(rf.model, test, test$order, input$evalthreshnum, 1,0, input$evalthresh))
#})

  output$errorlow <- renderPlot({
    error_decomposition(rf.model,test$order,"low",input$evalthreshnum,input$evalthresh)
})
  
  output$errorhigh <- renderPlot({
    error_decomposition(rf.model,test$order,"high",input$evalthreshnum,input$evalthresh)
  })
 # output$evalerroroutput <- renderPrint({
 #   capture.output(rf.model,test$order,"high",input$evalthreshnum,input$evalthresh)
 # })


            #####INTERPRETATION TAB#####

#  output$varimp <- renderPlot({
      
 #   varimp(rf.model)
#  })

  output$pdp <- renderPlot({
    pdp (rf.model,data)
  })
  output$ice1 <- renderPlot({
   #ice(input$icevar)
    ice(pidICEbox)
  })
  output$ice2 <- renderPlot({
    #ice(input$icevar)
    ice(priceICEbox)
  })
}

