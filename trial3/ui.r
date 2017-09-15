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
    numericInput(inputId = "obs",min = 1, max = 40, step = 1,label = "Number of observations to view:",value = 5)
    ),
  
  mainPanel(
    h2("Data Overview"),
    tableOutput("datatable"), 
    
 #   h2("Data Summary"),
 #   verbatimTextOutput("summarydata"),
    h2("Model Summary"),
    verbatimTextOutput("summarymodel"))
  ),
                   
                   #####MODEL SELECTION TAB#####
   
  tabPanel("Model Selection", 
           
  titlePanel("Model Selection"), 
  
  sidebarPanel(p("This tab offers three visualizations that are aiming to...")),
  
  mainPanel(
    h2("ROC Curve"),
    verbatimTextOutput("cutoffpoint"), 
    plotOutput("roccurve"),
    h2("PR Curve"),
    plotOutput("prcurve")),
    h2("Lift Curve"),
    plotOutput("liftcurve")
  
  ),
                   
                   #####VARIABLE SELECTION TAB#####
  tabPanel("Variable Selection",
            
  titlePanel("Variable Selection"), 
  
  sidebarPanel(selectInput(inputId = "method", label ="Select method:", choices = c("RF", "Boruta_Tentative","Boruta_Final"))
   ), 
  
  mainPanel(p("Here goes the result"), plotOutput("vs"))
   ),
                   
                   
                   #####EVALUATION TAB#####
  
  tabPanel("Evaluation",
           
  titlePanel("Jay"), 
  
  sidebarPanel(h1 ("Png for now, will be  for realz")),
  
  mainPanel(h2("Confusion matrix"), 
            img(src = "Rplot.png"),
            h2("Error deconmposition"),
            img(src = 'rplot01.png'))
  
  ),
                   
                   
                   #####INTERPRETATION TAB#####
  
  tabPanel("Interpretation",
           
  titlePanel("Looking into 'black box'"), 
  
  mainPanel(
    p("Graphs are loading, give it some time"), 
    h2("Variable Importance plot"),
    plotOutput("varimp"), 
    h2("Partial Dependance Plots of most important variables"),
    plotOutput("pdp"),
    h2("ICEbox plots"),
    plotOutput("ice")
                                      
  ))))
