library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = "Information Based Sub-Sampler",
    theme = shinythemes::shinytheme("cerulean"),
    # self-introduction page
    tabPanel(title = "Read Me",
             h3("Introduction to IBOSS", align = 'center'),
             tags$hr()
             ),
    
    # demo page
    tabPanel(title = "Demos", 
             h3("Demonstration", align = 'center')),

    # application page
    tabPanel(title = "Application")
    
  )
)
