#' Wrapper function to demonstrate examples in paper
#' 
#' @export
demoIboss <- function(n=NULL, k=NULL){
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
  if(is.null(n) | is.null(k)){
             n <- c(5000, 10^4, 10^5, 10^6)
             k <- 1000
             p <- 50
  }
  
  
  cat("Initializing...\n")
  
  mse0_mat <- mse1_mat <- matrix(0, ncol= length(k), nrow = length(n))
  colnames(mse_mat) <- paste0("k=",k)
  rownames(mse_mat) <- paste0("n=",n)
  for(ni in n){
    for(ki in k){
      tmp <- get_mse(case, ni, ki, p, compare=TRUE)
    }
  }
  
    
}






