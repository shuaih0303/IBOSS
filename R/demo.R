#' Wrapper function to demonstrate examples in paper
#' 
#' @export
demoIboss <- function(){
  cat("Please select an example to demonstate, or enter 0 to exit:\n")
  cases <- c("Sample Size: 5000, 10^4, 10^5, 10^6\nDistribution: multivariate normal\nSubsample Size: \n",
             "Sample Size: 5000, 10^4, 10^5, 10^6\nDistribution: multivariate lognormal\nSubsample Size: \n",
             "Sample Size: 5000, 10^4, 10^5, 10^6\nDistribution: multivariate t(2)\nSubsample Size: \n",
             "Sample Size: 5000, 10^4, 10^5, 10^6\nDistribution: mixture N(0, sigma) + t(2) + t(3) + Unif(0,2) + LN(0, sigma)\nSubsample Size: \n",
             "Sample Size: 5000, 10^4, 10^5, 10^6\nDistribution: multivariate normal random variables with interactions and quadratic terms.\nSubsample Size: \n",
             "Food Intake Data\n",
             "Chemical Sensor Data\n"
  )
  case <- menu(cases)
  
  cat("Initializing...\n")
  
  
    
}






