
############################################################
'CLAVIS APP'
############################################################



#Load data (!)
#data <- readRDS("fold_1_b2.rds")
boruta.train<- readRDS("boruta.train.RDS")
rfe.train <- readRDS("rfe.train.RDS")
data <- readRDS("alt")
rf.model <- readRDS("rfmodel")

#Libraries
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("ICEbox")) install.packages("ICEbox"); library("ICEbox")
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("Boruta")) install.packages("Boruta"); library("Boruta")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("plotROC")) install.packages("plotROC"); library("plotROC")
if(!require("InformationValue")) install.packages("InformationValue"); library("InformationValue")
if(!require("PRROC")) install.packages("PRROC"); library("PRROC")
if(!require("DT")) install.packages("DT"); library("DT")


#setwd("/Users/coldbear/Desktop/APAPaper/APA-project/trial3")



#Load  functionality
#source("dataexploration.r")
source("modelselection.r")
source("varselection.r")
#source("evaluation.r")
#source("interpretation.r")

#Start of server function

server <- function(input, output) {
  
  
           #####DATA  EXPLORATION TAB#####
# Load data observations
  output$datatable <- renderTable({
  head(data, n = input$obs)
})
  
# Generate a summary of the preloaded model
output$summary <- renderPrint({
  model = rf.model
  summary(model)
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




            #####INTERPRETATION TAB#####

  output$varimp <- renderPlot({
    varimp(model)
  })
  output$pdp <- renderPlot({
    pdp (model,data)
  })
}

