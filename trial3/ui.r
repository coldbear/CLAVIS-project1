############################################################
'CLAVIS APP'
############################################################


#### User interface #####

shinyUI(navbarPage(title=strong("CLAVIS"),
                   
                   #####DATA  EXPLORATION TAB#####
                
  tabPanel("Data",
           
  titlePanel("Data Exploration"), 
  
  sidebarPanel(
    p("The  application comes with pre-loaded dataset from APA and trained RandomForest Model"),
    numericInput(inputId = "obs",min = 1, max = 40, step = 2,label = "Number of observations to view:",value = 10)
    ),
  
  mainPanel(
    tableOutput("datatable"), 
    p("Model Summary"),
    verbatimTextOutput("summary"))
  ),
                   
                   #####MODEL SELECTION TAB#####
   
  tabPanel("Model Selection", 
           
  titlePanel("Model Selection"), 
  
  sidebarPanel(h1("Description of curves")),
  
  mainPanel(
    h1("ROC Curve"),
    verbatimTextOutput("cutoffpoint"), 
    plotOutput("roccurve"),
    h1("PR Curve"),
    plotOutput("prcurve")),
    h1("Lift Curve"),
    plotOutput("liftcurve")
  
   ),
                   
                   #####VARIABLE SELECTION TAB#####
  tabPanel("Variable Selection",
            
  titlePanel("Variable Selection"), 
  
  sidebarPanel(selectInput(inputId = "method", label ="Select method:", choices = c("RF", "Boruta_Tentative", "Boruta_Final"))
   ), 
  
  mainPanel(p("Here goes the result"), plotOutput("vs"))
   ),
                   
                   
                   #####EVALUATION TAB#####
  
  tabPanel("Evaluation",
           
  titlePanel("Jay"), 
  
  sidebarPanel(h1 ("Select parameters")),
  
  mainPanel(p("Here goes evaluation"), plotOutput("STUFF"))
  
  ),
                   
                   
                   #####INTERPRETATION TAB#####
  
  tabPanel("Interpretation",
           
  titlePanel("Let's take a look into 'black box'"), 
  
  mainPanel(p("Here goes the data exploration (it's loading, give it time, the progress bar will  be added)"), plotOutput("varimp"), plotOutput("pdp")
                                      
                                      
  ))))
