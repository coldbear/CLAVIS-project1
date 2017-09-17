############################################################
'CLAVIS APP'
############################################################


#### User interface #####

shinyUI(navbarPage(title=strong("CLAVIS"),
                   
  tabPanel("About",
           
  titlePanel("About CLAVIS"),
  sidebarPanel(p("The CLAVIS applcation was created on the basis of Shiny R Package, as part of the Applied Predictive Analytics course assignment at Humboldt University of Berlin."),
               h4("Team of Authors:"),
               tags$li("Jayalakshmi Choodamani"), 
               tags$li("Stephanie Finkenwirth"), 
               tags$li("Hamayun Khan"),
               tags$li("Alisa Kolesnikova"),
               hr(),
               print("Copyright 2017  kolesnal@hu-berlin.de")),  
    
  mainPanel(h2("Application Description"),
             p("The primary goal of the application is provision of CLAssifier VISualization infrastructure for the complimentary usage when performing a task of classification analysis. Authors strongly believe in efficiency and utility of visual perception when going through the classic KDD process. The appliation consists of 5 tabls:"),
            h4("Data Exploration:"),
            p("contains brief overview of the dataset, scatterplot  depicting the intervariable  interaction, class disctributions across key varibles, boxplot data distributions and the table of missing  values"),
            h4("Model Selection:"),
            p("offers three graphic options depicting the model output and allows to experiment with cost function effect on optimal cutoff  point"), 
            h4("Variable Selection:"),
            p("provides  three graphic expressions of Random Forest, Boruta Tentative and Boruta General variable selection methods"),
            h4("Model  Evaluation:"),
            p("contains visualised confusion matrix and error decomposition plot, allowing to explore effects of different  threshold values"),
            h4("Interpretation:"),
            p("addressed the internal 'chemistry' of  the models, offering a look into the black box using Partial Dependende and ICEbox plots"))
             ),
           
                   #####DATA  EXPLORATION TAB#####
                
  tabPanel("Data Exploration",
           
  titlePanel("Data Exploration"), 
  
  sidebarPanel(
    p("The  application comes with pre-loaded dataset from APA and trained RandomForest Model"),
    numericInput(inputId = "obs",min = 1, max = 40, step = 1,label = "Number of observations to view:",value = 3)
  #  useShinyjs(),
   # actionButton("scatt", "Scatterplot")
    
   # checkboxGroupInput("checkGroup", label = h3("Dataset Features"), 
                #       choices = feature.list, inline = F,
                  #     selected = names(feature.list))
    ),
  
  mainPanel(
    tableOutput("datatable"), 
    h2("Scatterplot"),
    plotOutput("exploration1"),
    h2("Class distribution"),
    plotOutput("exploration2"),
    h2("Boxplots"),
    plotOutput("boxplots"),
    h2("Missing values"),
    plotOutput("miss"),
    h2("Model Summary"),
    verbatimTextOutput("summarymodel"))
  ),
                   
                   #####MODEL SELECTION TAB#####
   
  tabPanel("Model Selection", 
         
  titlePanel("Model Selection"), 
  
  sidebarPanel(selectInput(inputId = "plottype", label ="Select type of plot:", choices = c("ROC-Curve", "PR-Curve", "Uplift-Curve")),
               numericInput(inputId = "CBTN", step = 1,label = "Cost Benefit True Negative:",value = 0),
               numericInput(inputId = "CBFN",step = 1,label = "Cost Benefit False Negative:",value = -1),
               numericInput(inputId = "CBFP",step = 1,label = "Cost Benefit False Positive:",value = -1),
               numericInput(inputId = "CBTP",step = 1,label = "Cost Benefit True Positive:",value = 0),
               actionButton("refreshButton", "Refresh")
               ),
  mainPanel(
    verbatimTextOutput("modelselecttext"), 
    plotOutput("modelselect")
  )),
                   
                   #####VARIABLE SELECTION TAB#####
  tabPanel("Variable Selection",
            
  titlePanel("Variable Selection"), 
  
  sidebarPanel(selectInput(
    inputId = "method", label ="Select method:", choices = c("Random_Forest", "Boruta_Tentative","Boruta_Final")),
    textOutput("vstext")
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
              numericInput("th",min = 0, max = 1, step=0.05,label = "Select threshold:",value = 0.6)
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
           
  titlePanel("Interpretation'"), 
  
  sidebarPanel(
    h2("Partial Dependance Plots"),
    p("help visualizing the average partial relationships between predicted response and one or more features, which allows to draw certain conclusions about the underlying relationships"),
    h2("ICEbox plots"),
    p("provide deeper look into the variance of the fitted value across the covariates, showing the points of occurring heterogeneity. It  takes individual observations into account  instead of averaged like PDP")),
    #selectInput(inputId = "icevar", label ="Select ICEbox variable:", choices = c("pid"="pidICEbox", "price"="priceICEbox"))),
  
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
