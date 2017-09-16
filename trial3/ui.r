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
    numericInput(inputId = "obs",min = 1, max = 40, step = 1,label = "Number of observations to view:",value = 3)
    ),
  
  mainPanel(
    h2("Data Overview"),
    tableOutput("datatable"), 
    plotOutput("exploration1"),
    plotOutput("exploration2"),
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
  
  sidebarPanel(selectInput(
    inputId = "method", label ="Select method:", choices = c("RF", "Boruta_Tentative","Boruta_Final")),
    h2("RF variable selection"),
    p("The plot shows how the RMSE changes as we increase the number of variables,the variables corresponding to the lowest RMSE are 'selected' and can be found using the 'predictors' function."),
    h2("Tentative Boruta Explanation"),
    p("Blue boxplots correspond to minimal, average and maximum Z score of a 
        shadow attribute. Red, yello and green boxplots represent Z scores of
      respectively rejected, tentatively confirmed and confirmed attributes."),
    h2("Final Boruta Explanation"),
    p("Blue boxplots correspond to minimum, average and maximum Z score of a 
        shadow attribute. Red and green boxplots represent Z scores of
    respectively rejected and confirmed attributes")
   ), 
  
  mainPanel(
    plotOutput("vs",  width = "100%", height = "800px")
   )),
                   
                   
                   #####EVALUATION TAB#####
  
  tabPanel("Evaluation",
           
  titlePanel("Model  Evaluation"), 
  
 sidebarPanel(selectInput(inputId = "evalthresh", label ="Select threshold:", choices = c("Random", "Mean", "Median", "3quad"))
           ),
  
  mainPanel(h2("Confusion matrix and error decompositon"), 
            plotOutput("eval"),
            verbatimTextOutput("evaloutput"))
  ),

                   
                   
                   #####INTERPRETATION TAB#####
  
  tabPanel("Interpretation",
           
  titlePanel("Looking into 'black box'"), 
  
  sidebarPanel(selectInput(inputId = "icevar", label ="Select ICEbox graph:", choices = c("pidICEbox", "priceICEbox"))),
  
  
  mainPanel(
    p("Graphs are loading, give it some time"), 
    h2("Variable Importance plot"),
    plotOutput("varimp"), 
    h2("Partial Dependance Plots of most important variables"),
    plotOutput("pdp"),
    h2("ICEbox plots"),
    plotOutput("ice"))
  )
 )
)
  
