#' App that run Demos and Simulations
#' @examples 
#' \dontrun{
#' # to launch the app
#' runIboss()
#' }
#' @export
runIboss <- function(){
  shiny::runApp(system.file('ibossapp', package = 'IBOSS'))
}


#' Get IBOSS Subsample from user input data
#' @describeIn getIBOSS0
#' @param response.included logical. If TRUE, repsonse vector is included in dataset. default is false.
#' @export
get_Iboss <- function(data, k, y_pos = 0, response.included=F){
  if (response.included & y_pos == 0) {
    stop("Please specify which column is response?")
  }
  
  return(getIBOSS0(data, k, y_pos))
}



#' Wapper of pre-designed examples.
#' @param ex integer, menu based.
#' @return MSEs and plots
#' @examples 
#' \dontrun{
#' # use menu based selector
#' runIbossExample()
#' # or input case number by user
#' runIbossExample(1)
#' }
#' @export
runIbossExample <- function(ex = NULL){
  ex <- menu(c(
    "fixed subsample size: 1000,\nflexible full data size: 5000, 10^4, 10^5, 10^6\nrepeat times: 1000\nreturn MSEs, plots and cpu times\n",
    "flexible subsample size: 200, 400, 500, 1000, 2000, 3000, 5000\nfixed subsample size: 10^6\nrepeat times: 1000\nreturn MSEs, plots and cpu times\n"
    #"cpu times on grid of\nfull data size: 5000, 50000, 500000\nnumber of predictors: 500\npredictor distribution: normal\n",
    #"cpu times on grid of\nfull data size: 500000\nNumber of predictors: 10, 100, 500\npredictor distribution: normal\n"
    )
  )
  
  cases <- c("Distribution: multivariate normal\n",
             "Distribution: multivariate lognormal\n",
             "Distribution: multivariate t(2)\n",
             "Distribution: mixture N(0, sigma) + t(2) + t(3) + Unif(0,2) + LN(0, sigma)\n",
             "Distribution: multivariate normal random variables with interactions and quadratic terms.\n"
  )
  
  
  # if ex is 1 or 2, choose a distribution of predictors ----
  if (ex == 1 | ex == 2) {
    case <- menu(cases) 
  }
  
  if (ex == 1) {
    n = c(5000, 10^4, 10^5, 10^6)
    p = 50
    k = 1000
    rept = 1000
    tmp <- demoIboss(case = case, n = n, k = k, p = p, rept = rept, compare = T)
    plot(tmp$mse0_plot)
    plot(tmp$mse1_plot)
    tmp$N <- n
    tmp$k <- k
    tmp$p <- p
    tmp$rept <- rept
    return(tmp)
  }
  
  if (ex == 2) {
    n = c(10^6)
    p = 50
    k = c(200, 400, 500, 1000, 2000, 3000, 5000)
    rept = 1000
    tmp <- demoIboss(case = case, n = n, k = k, p = p, rept = rept, compare = T)
    plot(tmp$mse0_plot)
    plot(tmp$mse1_plot)
    tmp$N <- n
    tmp$k <- k
    tmp$p <- p
    tmp$rept <- rept
    return(tmp)
  }
  
  return(ex)
}