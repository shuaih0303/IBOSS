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
    
    # demo page
    tabPanel(value="demo", title = "Demos", 
             titlePanel("Demonstration"),
             selectInput(inputId = "k", label="Subsample Size", choices = c("a","b","c")))
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
                   
                   downloadLink("download", "Download")
                   
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