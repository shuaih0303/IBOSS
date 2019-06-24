# script to generate all full data set in simulation section

#' Simulation Case Dataset Generator
#' @param case integer. Case number for simulation.
#' @param n integer. Sample size of full data.
#' @param p integer. Number of predictors excluding intercept.
#' @param sigmax matrix. Covariance matrix of predicotrs, default is auto-correlated.
Generate_Case_Full_Data <- function(case, n, p, sigmax=NULL){
  #set.seed(123)
  cpu_time_simulation <- system.time({
    
    d <- p
    if (is.null(sigmax))
    {
      
      corr  <- 0.5
      
      sigmax <- matrix(corr, d, d) + diag(1 - corr, d)
      
    }
    beta.0 = rep(1, d)
    
    switch(case,
           {## 1 normal mean 0, cov=sigmax
             X  <- mvtnorm::rmvnorm(n, rep(0, d), sigmax)
           }, {## 2 lognormal mean 0, cov=sigmax
             X  <- mvtnorm::rmvnorm(n, rep(0, d), sigmax)
             X <- exp(X)
           }, {## 3 t2 mean 0, cov=sigmax
             X  <- mnormt::rmt(n, rep(0, d), sigmax, 2)
           }, {## 4 mix of 5 different distributions
             X1 <- mvtnorm::rmvnorm(n/5, rep(1, d), sigmax) # normal
             X2 <- mnormt::rmt(n/5, rep(1, d), sigmax, 3) # t3
             X3 <- mnormt::rmt(n/5, rep(1, d), sigmax, 2) # t2
             X4 <- matrix(runif(n/5*d, 0, 2), n/5, d) # unif(0, 2)
             X5 <- mvtnorm::rmvnorm(n/5, rep(0, d), sigmax)
             X5 <- exp(X5) # log-normal
             X <- rbind(X1, X2, X3, X4, X5)
             rm(list = c("X1", "X2", "X3", "X4", "X5"))
           }, {## 17 interaction mean 0
             sigmax <- sigmax[1:20, 1:20]
             X <- mvtnorm::rmvnorm(n, rep(0, 20), sigmax)
             X <- cbind(X, X[,1]*X, X[,2]*X[,11:20])
           })
  
  X <- cbind(1, X)
  beta0 <- c(1, beta.0)
  Y  <- c(X %*% beta0) + rnorm(n, 0, 3)
  })
  
  
  cpu_time_fit <- system.time({
  Xsvd <- svd(X)
  U <- Xsvd$u
  L <- Xsvd$d
  V <- Xsvd$v
  beta.all <- V %*% (t(V)/L^2) %*% t(X) %*% Y
  })
  
  mse <- rep(0, 2)
  mse[1] <- (beta.all[1] - beta0[1])^2
  mse[2] <- sum((beta.all[-1] - beta0[-1])^2)
  
  
  names(cpu_time_simulation) <- NULL
  
  return(list(N = n, Y = Y, X = X, beta = beta.all, betaTrue = beta0, mse = mse, cpu_time_simulation=cpu_time_simulation, cpu_time_fit = cpu_time_fit))
}
