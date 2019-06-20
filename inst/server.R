#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  # set initial values
  x <- reactiveValues(out=list())
  
  
  #### Application page, uploading running iboss and etc
  observeEvent(req(input$file1),{ 
  
  x$out$dataInput <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
  
  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown 
    if(input$disp == "head") {
      return(head(x$out$dataInput))
    }
    else {
      return(x$out$dataInput)
    }
  })

  output$summary <- renderText({ 
    as.character(c("Dataset Info:\n", "Rows:", nrow(x$out$dataInput),"\n", "Columns:", ncol(x$out$dataInput), "\n", "Predictor Names:", colnames(x$out$dataInput),"\n","Rows that containing Missing Values:", sum(!complete.cases(x$out$dataInput))))
  })
  
})
  
  # get iboss and estimates 
  observeEvent(input$run_iboss,
               {
                 print(input$kk)
                 print(input$y_pos)
                 x$out$iboss_data <- getIBOSS0(x$out$dataInput, input$kk,input$y_pos)
                 output$iboss_data <- renderDataTable({
                   x$out$iboss_data$data
                 })
                 output$iboss_summary <- renderText({
                    c("Sampling Time:", round(x$out$iboss_data$cpu_time_sample[2], digits = 8))
                 })
                 # download iboss data
                 output$download <- downloadHandler(
                   filename = function() {
                     paste("iboss", "_size_of_",input$kk, ".csv", sep = "")
                   },
                   content = function(file) {
                     write.csv(x$out$iboss_data$data, file)
                   }
                 ) 
                 
               })
  

})
