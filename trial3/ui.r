############################################################
'CLAVIS APP'
############################################################


#### User interface #####

shinyUI(navbarPage(title=strong("CLAVIS"),
                   
                   #####DATA  EXPLORATION TAB#####
                
  tabPanel("Data Exploration",
           
  titlePanel("Data Exploration"), 
  
  sidebarPanel(
    p("The  application comes with pre-loaded dataset from APA and trained RandomForest Model"),
    numericInput(inputId = "obs",min = 1, max = 40, step = 1,label = "Number of observations to view:",value = 3)
    ),
  
  mainPanel(
    h2("Data Overview"),
    tableOutput("datatable"), 
    plotOutput("exploration1"),
    h2("Class distribution"),
    plotOutput("exploration2"),
    h2("Boxplots"),
    plotOutput("boxplots"),
    h2("Model Summary"),
    verbatimTextOutput("summarymodel"))
  ),
                   
                   #####MODEL SELECTION TAB#####
   
  tabPanel("Model Selection", 
         
  titlePanel("Model Selection"), 
  
  sidebarPanel(p("This tab offers three visualizations that are aiming to..."),
               selectInput(inputId = "plottype", label ="Select type of plot:", choices = c("ROC-Curve", "PR-Curve", "Uplift-Curve")),
               numericInput(inputId = "CBTN", step = 1,label = "Cost Benefit True Negative:",value = 0),
               numericInput(inputId = "CBFN",step = 1,label = "Cost Benefit False Negative:",value = -1),
               numericInput(inputId = "CBFP",step = 1,label = "Cost Benefit False Positive:",value = -1),
               numericInput(inputId = "CBTP",step = 1,label = "Cost Benefit True Positive:",value = 0)
               ),
  mainPanel(
    verbatimTextOutput("modelselecttext"), 
    plotOutput("modelselect")
  )),
                   
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
  #return_confmat(rf.model, test, test$order, input$evalthreshnum, 1,0, input$evalthresh)
  tabPanel("Evaluation",
           
  titlePanel("Model  Evaluation"), 
  
 sidebarPanel(h2("Select classification threshold:"),
          #    radioButtons(inputId = "input$evalthresh",list("Random", "Mean", "Median", "3quad")),
              numericInput(inputId = "input$evalthresh",min = 0, max = 1, step =0.05,label = "or custom threshold:",value = 0.6)
           ),
  
  mainPanel(h2("Confusion matrix"), 
            plotOutput("confmatrix"),
            #verbatimTextOutput("confmatrixoutput"),
            h2("Error Decomposition"),
            plotOutput("errorlow"),
            plotOutput("errorhigh")
            #verbatimTextOutput("evalerroroutput")
            )
  ),

                   
                   
                   #####INTERPRETATION TAB#####
  
  tabPanel("Interpretation",
           
  titlePanel("Looking into 'black box'"), 
  
  sidebarPanel(
    h2("Partial Dependance Plots"),
    p("help visualizing the average partial relationships between predicted response and one or more features, which allows to draw certain conclusions about the underlying relationships"),
    h2("ICEbox plots"),
    p("provide deeper look into the variance of the fitted value across the covariates, showing the points of occurring heterogeneity. It  takes individual observations into account  instead of averaged like PDP")),
    #selectInput(inputId = "icevar", label ="Select ICEbox graph:", choices = c("pidICEbox", "priceICEbox"))),
  
  mainPanel(
    p("Graphs are loading, give it some time"), 
  #  h2("Variable Importance plot"),
  #  plotOutput("varimp"), 
    h2("Partial Dependance Plots of most important variables"),
    plotOutput("pdp"),
    h2("ICEbox plots"),
    plotOutput("ice1"),
    plotOutput("ice2"))
  )
 )
)
