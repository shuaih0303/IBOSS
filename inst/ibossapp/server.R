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
  
  
#  observeEvent(input$case,{
#    x$out$sim <- demoFigures(figNum = input$case)
#    output$caseplot <- x$out$sim$
#  })
  
  
  # Simulation Page, run simulations
  
  observeEvent(input$run_sim_example,{
    withProgress(message="Initializing...", value = 0, {
      incProgress(0.1, message = "Simulation in Progress...")
    x$out$sim_output <- demoIboss(as.numeric(input$sim_case),as.numeric(strsplit(input$sim_n,split = ',')[[1]]), as.numeric(strsplit(input$sim_k,split = ',')[[1]]), as.numeric(strsplit(input$sim_p,split = ',')[[1]]), as.numeric(input$sim_rept), as.logical(input$sim_compare))
    
      incProgress(0.7, message = "Finishing simulations...")
      incProgress(0.8, message = "Rendering output...")
    
    output$sim_mse0 <- renderTable(x$out$sim_output$mse0, rownames = T, digits = 8, bordered = T, striped = T)
    output$sim_mse1 <- renderTable(x$out$sim_output$mse1, rownames = T, digits = 8, bordered = T, striped = T)
    output$sim_cpu_time <- renderTable(x$out$sim_output$cpu_time, rownames = T, digits = 4, bordered = T, striped = T)
    
    x$out$plot_mse <- c(length(as.numeric(strsplit(input$sim_k,split = ',')[[1]]))>1 & length(as.numeric(strsplit(input$sim_n,split = ',')[[1]]))>1 & !as.logical(input$sim_compare))
    
    if(x$out$plot_mse){
      output$sim_plot_mse0 <- output$sim_plot_mse1 <- NULL
    }else{
    output$sim_plot_mse0 <- renderPlot(plot(x$out$sim_output$mse0_plot))
    output$sim_plot_mse1 <- renderPlot(plot(x$out$sim_output$mse1_plot))}
      incProgress(0.9, message = "Printing output into console...") 
      print(x$out$sim_output)
      incProgress(1, message = "Done!")
    })
  })
  
  
  #### Application page, uploading running iboss and etc
  observeEvent(req(input$file1),{ 
  
  x$out$dataInput <- read.table(input$file1$datapath,
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
  
  
  observeEvent(input$y_pos,{
  
  x$out$resp <- ifelse(input$y_pos > 0, colnames(x$out$dataInput)[input$y_pos], "not included")
  
  output$summary <- renderText({ 
    as.character(c("Dataset Info:\n", "Rows:", nrow(x$out$dataInput),"\n", "Columns:", ncol(x$out$dataInput), "\n", "Column Names:", colnames(x$out$dataInput),"\n", "Response:", x$out$resp,"\n","Rows that containing Missing Values:", sum(!complete.cases(x$out$dataInput))))
  })
  })
})
  
  # get iboss and estimates 
  observeEvent(input$run_iboss,
               {
                 print(input$kk)
                 print(input$y_pos)
                 x$out$iboss_data <- getIBOSS0(x$out$dataInput, input$kk,input$y_pos)
                 output$iboss_data <- renderDataTable({
                   head(x$out$iboss_data$data)
                 })
                 
                 output$iboss_summary <- renderText({ 
                   as.character(c("Sub-dataset Info:\n", "Rows:", nrow(x$out$iboss_data$data),"\n", "Columns:", ncol(x$out$iboss_data$data), "\n", "Column Names:", colnames(x$out$iboss_data$data),"\n", "Response:", x$out$resp,"\n","Sampling Time:", round(x$out$iboss_data$cpu_time_sample[2], digits = 4), "second(s)."))
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
