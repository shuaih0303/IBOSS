#' IBOSS Package demonstration

#' Run Demos and Simulations
runIboss <- function(){
  shiny::runApp(system.file('ibossapp', package = 'IBOSS'))
}


#' Get IBOSS Subsample from user input data
#' @describeIn getIBOSS0
#' @param response.included logical. If TRUE, repsonse vector is included in dataset. default is false.
#' @export
get_Iboss <- function(data, k, y_pos = 0, response.included=F){
  if (response.included & y_pos == 0){
    stop("Please specify which column is response?")
  }
  
  return(getIBOSS0(data, k, y_pos))
}

