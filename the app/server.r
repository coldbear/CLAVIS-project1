
############################################################
'CLAVIS APP'
############################################################


#Load data 
data <- readRDS("workset")
rf.model <- readRDS("rf.model")
train <- readRDS("train2")
test<- readRDS("test2")


#Libraries
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("shinyjs")) install.packages("shinyjs"); library("shinyjs")
if(!require("ICEbox")) install.packages("ICEbox"); library("ICEbox")
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("Amelia")) install.packages("Amelia"); library("Amelia")
if(!require("utils")) install.packages("caret"); library("utils")
if(!require("Boruta")) install.packages("Boruta"); library("Boruta")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("reshape2")) install.packages("reshape2"); library("reshape2")
if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
if(!require("e1071")) install.packages("e1071"); library("e1071")
if(!require("InformationValue")) install.packages("InformationValue"); library("InformationValue")
if(!require("PRROC")) install.packages("PRROC"); library("PRROC")



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
#observeEvent(input$scatt, {
#  toggle("exploration1")
#})

output$exploration2 <- renderPlot({
  explora2(train)
})

output$boxplots <- renderPlot({
  get_boxplots(train)
})

output$miss <- renderPlot({
  missing(original)
})

           #####MODEL SELECTION TAB#####
output$modelselecttext <- renderPrint({
  create_plots("order", rf.model, train, test,CBTN = input$CBTN, CBFN = input$CBFN, CBFP = input$CBFP, CBTP = input$CBTP, input$plottype)
})
output$modelselect <- renderPlot({
  input$refreshButton
  isolate({
  create_plots("order", rf.model, train, test,CBTN = input$CBTN, CBFN = input$CBFN, CBFP = input$CBFP, CBTP = input$CBTP, input$plottype)})
})


         #####VARIABLE SELECTION TAB#####

#Load the variable selection plotsplots

output$vs <- renderPlot({
  Select_Features(input$method)
})

output$vstext <- renderPrint({
  annotation(input$method)
  })



            #####EVALUATION TAB#####

output$confmatrix <- renderPlot({
  return_confmat(rf.model, test$order, input$th, 1,0)
})   

  output$errorlow <- renderPlot({
    error_decomposition(rf.model,test$order,"low",input$th)
})
  
  output$errorhigh <- renderPlot({
    error_decomposition(rf.model,test$order,"high",input$th)
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
  #  ice(input$icevar)
    ice(priceICEbox)
  })
}

