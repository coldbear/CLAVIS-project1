############################################################
'CLAVIS APP'
############################################################


# This is the user-interface definition of a Shiny web application.

shinyUI(navbarPage(title=strong("CLAVIS"),
                
  tabPanel("Data Overview",
                            
  titlePanel("Loading DataSet"), 
                            sidebarPanel(p("The  application comes with pre-loaded dataset from APA and trained RandomForest Model"),numericInput(inputId = "obs",
                                           label = "Number of observations to view:",
                                           value = 10)),
  mainPanel(p("Model Summary"),verbatimTextOutput("summary"))),
                   
                   #####NEXT TAB#####
                   tabPanel("Model Selection", 
                            titlePanel("Stephanie"), 
                            sidebarPanel(h1("Boom")),
                            mainPanel(p("herennn"))
                   ),
                   
                   #####NEXT TAB#####
                   tabPanel("Variable Selection",
                            titlePanel("Hamayun"), 
                            sidebarPanel(selectInput(inputId = "method", label ="Select method:", choices = c("RF", "Boruta_Tentative", "Boruta_Final"))),     
                            mainPanel(p("Here goes the result"), plotOutput("vs"))
                   ),
                   
                   
                   #####NEXT TAB#####
                   tabPanel("Evaluation",
                            titlePanel("Jay"), 
                            sidebarPanel(h1 ("Select parameters")),     
                            mainPanel(p("Here goes evaluation"), plotOutput("STUFF"))
                   ),
                   
                   
                   #####NEXT TAB#####
                   tabPanel("Interpretation",
                            titlePanel("Let's take a look into 'black box'"), 
                            mainPanel(p("Here goes the data exploration (it's loading, give it time, the progress   bar will  be added)"), plotOutput("varimp"), plotOutput("pdp")
                                      
                                      
                            ))))
