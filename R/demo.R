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
  if (is.null(n) | is.null(k)){
             n <- c(5000, 10^4, 10^5, 10^6)
             k <- 1000
             p <- 50
  }
  
  
  cat("Initializing...\n")
  
  mse0_mat <- mse1_mat <- matrix(0, ncol= length(k), nrow = length(n))
  colnames(mse_mat) <- paste0("k=",k)
  rownames(mse_mat) <- paste0("n=",n)
  for (ni in n){
    for (ki in k){
      tmp <- getMSE(case, ni, ki, p, compare=TRUE)
    }
  }
  
    
}

demoFigures <- function(figNum=NULL){
  figs <- c("Comparisons of three subsampling methods in terms of MSEs\n with fixed subsample size 1000\n and ranging size of full data\n n=5000, 10^4, 10^5, 10^6\n",
            "Comparisons of three subsampling methods in terms of MSEs\n with fixed size of full data 10^6\n and ranging size of subsample size\n k = 200, 400, 500, 1000, 2000, 3000, 5000\n",
            "Comparisons of IBOSS and Full Data in terms of statistical inference\nPlots of coverage probabilities and average length of 95% confidence intervals\n against ranging size of full data\nn=5000, 10^4, 10^5, 10^6\nfixed subsample size k = 1000\n",
            "Food Intakes Data\n",
            "Chemical Sensor Data\n")
  if (is.null(figNum)) {
    figNum <- menu(figs)
  }
  
  cat("CASE SELECTED", figNum, "\n") 
  p <- 50
  
  switch(figNum, {
    # figure 1, comparisons of intercept and slopes
    N <- c(5000, 10^4, 10^5, 10^6)
    k <- 1000
    case <- 1:6
    p <- 50
    
    for (i in case) {
      mse0_all <- mse0_srs <- mse0_lev <- mse0_iboss <- matrix(0,nrow = max(case), ncol = length(N))
      mse1_all <- mse1_srs <- mse1_lev <- mse1_iboss <- matrix(0,nrow = max(case), ncol = length(N))
      for (Ni in 1:length(N)) {
        tmp <- getMSE(i, N[Ni], k, p, compare = T)
        mse0_all[i,Ni] <- tmp$mse0_all
        mse0_srs[i,Ni] <- tmp$mse0_srs
        mse0_lev[i,Ni] <- tmp$mse0_lev
        mse0_iboss[i,Ni] <- tmp$mse0_iboss
        mse1_all[i,Ni] <- tmp$mse0_all
        mse1_srs[i,Ni] <- tmp$mse0_srs
        mse1_lev[i,Ni] <- tmp$mse0_lev
        mse1_iboss[i,Ni] <- tmp$mse0_iboss
      }
    }
    
    mse0_plot <- ggplot2::ggplot()
    mse1_plot <- ggplot2::ggplot()
  })
  
  
  
  return(out)
  
}




