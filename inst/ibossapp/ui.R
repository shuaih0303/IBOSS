library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "SAIF EMBA 2019 Big Data Analysis Demonstration Information Based Sub-Sampler",
    theme = shinythemes::shinytheme("cerulean"),
    # self-introduction page
    tabPanel(value="intro", title = "Read Me",
             h3("Introduction to IBOSS", align = 'center'),
             tags$hr(),
             verticalLayout(
               withMathJax(),
               h4("Information Based Optimal Subsampling (IBOSS) supports multiple linear regression models:$$y_{ij} = \\beta_0 + \\sum_{j=1}^{p} x_{ij}\\beta_j + \\epsilon_{ij}$$"),
               tags$hr(),
               h4("Key functions:"),
               h5(tags$code("demoIboss"), "Built-in simulation wrappers."),
               h5(tags$code("get_Iboss"), "Returns IBOSS algorithm and return subsamples."),
               h5(tags$code("runIboss"), "Launch an shiny app that runs", tags$code("demoIboss"), "and",  tags$code("get_Iboss"), "."),
               tags$hr()
             )
             
             
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
                   helpText("If checked, Subsample Size and Full Data Size are allowed to be vectors separated by comma; otherwise, they are only allowed to be integer and vector combination."),
                   actionButton('run_sim_example', "Run Simulation!")
                 ),
                 mainPanel(
                   h4("Results"),
                   tabsetPanel(
                     tabPanel("MSE Slopes", 
                        fluidRow(column(8, align='center', tableOutput("sim_mse1"))),
                        fluidRow(column(8, align="center", plotOutput("sim_plot_mse1")
                       )
                     )
                        ),
                     tabPanel("MSE Intercept", 
                              fluidRow(column(8, align='center', tableOutput("sim_mse0"))),
                              fluidRow(column(8, align='center', plotOutput("sim_plot_mse0")))
),
                     tabPanel("Average Sampling Time", tableOutput("sim_cpu_time"), helpText("Numbers of first row are times that generating full data.")
                              )
                   )
                 )
                  
                 
               )
             )
             )
             ,
    # application page
    tabPanel(value="app", title = "Upload Data",
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
                   fluidRow(
                     column(4, offset = 0, radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ",")), 
                     column(4, offset = 0, radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"')),
                     column(4, offset = 0, radioButtons("disp", "Display",choices = c(Head = "head",All = "all"),selected = "head"))
                     ),
                   # Horizontal line ----
                   tags$hr(),
                   
                   numericInput(inputId="y_pos", label = "Which column is response?", value = 0),
                   helpText("0 means response is not included."),
                   numericInput(inputId="kk", label = "Subsample Size", value = 1000),
                   # Horizontal line ----
                   tags$hr(),
                   
                   # Input: Select number of rows to display ----

                   actionButton(inputId = "run_iboss", "RUN IBOSS"),
                   
                   downloadButton("download", "Download IBOSS")
                   
                 ),
                 # Main panel for displaying outputs ----
                 mainPanel(
                   # Output: Data file ----
                   dataTableOutput("contents"),
                   verbatimTextOutput("summary"),
                   dataTableOutput("iboss_data"),
                   verbatimTextOutput("iboss_summary"),
                   verbatimTextOutput("output")
                   #tableOutput("iboss_data_summary_1")
                 )
                 
               )
             )
             
             )
    )
)