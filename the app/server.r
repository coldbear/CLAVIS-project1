
############################################################
                      'CLAVIS APP'
############################################################

#Libraries
library(shiny)
library(caret)
library(randomForest)
#library(maps)
#library(mapproj)


#Load data
data <- readRDS("alt")

#Load  functionality
source("interpretation.R")

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  
  output$pdp <- renderPlot({
    varimp_pd (data = data, 
              train_size= input$slidertrainsplit, 
              var_clus="order", 
              rf_ntree=input$ntreeslider,
              rf_mtry=input$mtryslider)
  })
})

    