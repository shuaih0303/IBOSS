library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "Information Based Sub-Sampler",
    theme = shinythemes::shinytheme("cerulean"),
    # self-introduction page
    tabPanel(value="intro", title = "Read Me",
             h3("Introduction to IBOSS", align = 'center'),
             tags$hr()
             ),
    
    tabPanel(value="example", title = "Examples", h4("Examples in IBOSS paper", align = "center"), tags$hr(), 
        selectInput("case", "Please select an example:", c("Comparisons of three subsampling methods in terms of MSEs\n with fixed subsample size 1000\n and ranging size of full data\n n=5000, 10^4, 10^5, 10^6\n",
                                                                   "Comparisons of three subsampling methods in terms of MSEs\n with fixed size of full data 10^6\n and ranging size of subsample size\n k = 200, 400, 500, 1000, 2000, 3000, 5000\n",
                                                                   "Comparisons of IBOSS and Full Data in terms of statistical inference\nPlots of coverage probabilities and average length of 95% confidence intervals\n against ranging size of full data\nn=5000, 10^4, 10^5, 10^6\nfixed subsample size k = 1000\n",
                                                                   "Food Intakes Data\n",
                                                                   "Chemical Sensor Data\n"), selected = 1), mainPanel()
    ),
    
    
    # Simulation Page
    tabPanel(value="Simulations", title = "Simulations", 
             fluidPage(
               # App title ----
               titlePanel("Simulation Studies"),
               
               
               # Sidebar layout with input and output definitions
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   selectInput("sim_case", "Please select an example:", 
                               list("Normal"=1, "Lognormal"=2, "t(df=2)"=3, "Normal + t(df=2) + t(df=3) + lognormal + unif(0,2)"=4, "Normal main effects and interactions"=5), selected = 1),
                   textInput("sim_n", "Full Data Size", value = "1000,2000,3000"),
                   textInput("sim_p", "Number of Predictors", value = "50"),
                   textInput("sim_k", "Subsample Size", value = "1000"),
                   textInput("sim_rept", "Repetition", value = "10"),
                   checkboxInput("sim_compare", "Comparisons to FULL,UNI, and LEV?", value=TRUE),
                   actionButton('run_sim_example', "Run Simulation!")
                 ),
                 mainPanel(
                   h4("Results"),
                   tabsetPanel(
                     tabPanel("MSE Slopes", fluidRow(
                       column(8, align="center",
                              plotOutput("sim_plot_mse1")
                       )
                     ),
                        fluidRow(column(8, align='center', tableOutput("sim_mse1")))),
                     tabPanel("MSE Intercept", 
                              fluidRow(column(8, align='center', plotOutput("sim_plot_mse0"))),
                              fluidRow(column(8, align='center', tableOutput("sim_mse0")))),
                     tabPanel("Computing Time", dataTableOutput("sim_cpu_time"))
                   )
                 )
                  
                 
               )
             )
             )
             ,
    # application page
    tabPanel(value="app", title = "Application",
             fluidPage(
               
               # App title ----
               titlePanel("Uploading Files"),
               
               # Sidebar layout with input and output definitions ----
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Select a file ----
                   fileInput("file1", "Upload CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                   # Input: Checkbox if file has header ----
                   checkboxInput("header", "Dataset contains header", TRUE),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   numericInput(inputId="y_pos", label = "Response Column", value = 0),
                   numericInput(inputId="kk", label = "Subsample Size", value = 1000),
                   # Input: Select separator ----
                   radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   
                   # Input: Select quotes ----
                   radioButtons("quote", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"'),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Select number of rows to display ----
                   radioButtons("disp", "Display",
                                choices = c(Head = "head",
                                            All = "all"),
                                selected = "head"),
                   actionButton(inputId = "run_iboss", "RUN",),
                   
                   downloadButton("download", "Download")
                   
                 ),
                 # Main panel for displaying outputs ----
                 mainPanel(
                   # Output: Data file ----
                   dataTableOutput("contents"),
                   verbatimTextOutput("summary"),
                   dataTableOutput("iboss_data"),
                   verbatimTextOutput("iboss_summary"),
                   verbatimTextOutput("output")
                 )
                 
               )
             )
             
             )
    )
)