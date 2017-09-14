
############################################################
                          'CLAVIS APP'
############################################################


# This is the user-interface definition of a Shiny web application.

shinyUI(navbarPage(title=strong("CLAVIS"),

tabPanel("Data Overview",
                titlePanel("Check out your data"), 
                sidebarPanel(numericInput("obs", "Number of observations to view:", 5), sliderInput("slidertrainsplit","Proportion of Training observations",min = 0, max = 1, value = 0.7, step = 0.1),selectInput("algorithm", "Choose a Classification algorithm:", choices = c("rpart", "randomForest", "lda"))),
                 mainPanel(p("here goes the data exploration"))),
        #####NEXT TAB#####
tabPanel("Variable Selection", 
                titlePanel("And here are the vars"), sidebarPanel(h1("Boom")),mainPanel(p("herennn"))),
        
        
        #####NEXT TAB#####
tabPanel("Evaluation"),
        
        
        #####NEXT TAB#####
tabPanel("Interpretation",
        titlePanel("Let's take a look into 'black box'"), 
        sidebarPanel(sliderInput("slidertrainsplit","Proportion of Training observations",min = 0, max = 1, value = 0.3, step = 0.1), sliderInput("ntreeslider","Number of Trees",min = 150, max = 1400, value = 200, step = 100),sliderInput("mtryslider","No of mtry",min = 2, max = 20, value =12, step = 2)),
        mainPanel(p("Here goes the data exploration (it's loading, give it time, the progress   bar will  be added)"), plotOutput("varimp"), plotOutput("pdp"), plotOutput("ice"))
                 
)))
