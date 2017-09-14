
############################################################
'CLAVIS APP'
############################################################

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

#library(maps)
#library(mapproj)
#setwd("/Users/coldbear/Desktop/APAPaper/APA-project/trial3")
#Load data (!)
data <- readRDS("alt")
rf.model <- readRDS("rfmodel")
boruta.train = readRDS(file = "boruta.train.RDS")
rfe.train = readRDS(file = "rfe.train.RDS")

#Load  functionality
source("vaselection.r")
varimp <- function(model) {
  varImpPlot(model, type = 2)
}
pdp <- function(model, data) {
  par(mar=c(1,1,1,1))
  a <- partialPlot(model, data, pid)
  b <- partialPlot(model, data, X15.1.adFlag_last_1_day.Mat)
  c <-  partialPlot(model, data, X15.3.adFlag_last_1_obs.Mat)
  return(list(a,b,c))
}
server <- function(input, output) {
  
#Data Overview Tab

#Variable Selection Tab

output$vs <- renderPlot({
Select_Features(input$method)
})


# Generate a summary of the dataset ----
output$summary <- renderPrint({
  model = rf.model
  summary(model)
})


  #Interpretation
  output$varimp <- renderPlot({
    varimp(model)
  })
  output$pdp <- renderPlot({
    pdp (model,data)
  })
}

