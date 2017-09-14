
############################################################
'CLAVIS APP'
############################################################

#Libraries
library(shiny)
library(caret)
library(randomForest)
library(ICEbox)
library(psych)
library(caret)
library(Boruta)
library(ggplot2)
library(plotROC)
library(InformationValue)
library(PRROC)
#library(maps)
#library(mapproj)
#setwd("/Users/coldbear/Desktop/APAPaper/APA-project/trial3")
#Load data (!)
data <- readRDS("alt")
rf.model <- readRDS("rfmodel")
boruta.train = readRDS(file = "boruta.train.RDS")
rfe.train = readRDS(file = "rfe.train.RDS")

#Load  functionality
source("varselection.r")
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

