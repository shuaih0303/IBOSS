#' Function to get IBOSS sample for given dataset 
getIBOSS <- function(caseData, k){
  
  cpu_time_sample <- system.time({
  
  d <- length(caseData$beta) - 1
  r <- k / d / 2
  idx <- getIdx(caseData$X[,2], caseData$X[,2], as.integer(r), k)
  for (j in 3:d) {
    tmp <- getIdx(caseData$X[-idx,j], caseData$X[,j], as.integer(r), k)
    idx <- c(idx, tmp)
  }
  X <- caseData$X[idx,]
  Y <- caseData$Y[idx]
  })
  
  
  cpu_time_fit <- system.time({
  eig <- eigen(t(X) %*% X, symmetric = T)
  #iI.oD <- eig$vectors %*% (t(eig$vectors)/eig$values)
  beta <- eig$vectors %*% (t(eig$vectors)/eig$values) %*% t(X) %*% Y
  })

  mse <- rep(0, 2)
  mse[1] <- (beta[1] - caseData$beta[1])^2
  mse[2] <- sum((beta[-1] - caseData$beta[-1])^2)
  
  out <- list(N = caseData$N, X = X, Y = Y, beta = beta,
              betaTrue = caseData$beta, sample_size = k,
              mse = mse, cpu_time_sample = cpu_time_sample,
              cpu_time_fit = cpu_time_fit)
  
  return(out)
}


#' Function to get IBOSS sample only
#' @param data dataframe or data.table
#' @param k integer, desired subsample size 
#' @param y_pos integer, position of response column
#' @export
getIBOSS0 <- function(data, k, y_pos=0){
  if (y_pos > 0) {
  Y <- data[, y_pos]
  X <- data[, -y_pos]
  }else{
    X <- data
  }
  
  data_size <- dim(X)
  N <- data_size[1]
  
  cpu_time_sample <- system.time({
    
    d <- data_size[2] 
    r <- k / d / 2
    idx <- getIdx(X[,1], X[,1], as.integer(r), k)
    for (j in 3:d) {
      tmp <- getIdx(X[-idx,j], X[,j], as.integer(r), k)
      idx <- c(idx, tmp)
    }
    
    X <- X[idx,]
    if(y_pos > 0){
    Y <- Y[idx]
    outdata <- data.frame(X = X, Y = Y)
    }else{
      outdata <- data.frame(X = X)
    }
  })
  
  
  
  out <- list(N = N, data=outdata, sample_size = k,
              cpu_time_sample = cpu_time_sample)
  
  return(out)
}


#' Function to get leverage subsample
getLEV <- function(caseData, k){
  
  cpu_time_sample <- system.time({
  # calculating leverage score
  Xsvd <- svd(caseData$X)
  U <- Xsvd$u
  L <- Xsvd$d
  V <- Xsvd$v
  PI <- sqrt(rowSums(U^2))
  PI <- PI / sum(PI)
  PI.i <- 1 / PI
  
  # leverage sampling
  idx <- sample(caseData$N, k, T, PI)
  X <- caseData$X[idx,]
  Y <- caseData$Y[idx]
  })
  
  cpu_time_fit <- system.time({
  pi.i <- PI.i[idx]
  eig <- eigen(t(X) %*% (pi.i * X), symmetric = T)
  #iI <- eig$vectors %*% (t(eig$vectors)/eig$values)
  beta <- eig$vectors %*% (t(eig$vectors)/eig$values) %*% t(X) %*% (pi.i * Y)
  })
  
  mse <- rep(0, 2)
  mse[1] <- (beta[1] - caseData$beta[1])^2
  mse[2] <- sum((beta[-1]-caseData$beta[-1])^2)
  
  
  out <- list(X=X, Y=Y, beta=beta, betaTrue = caseData$beta, sample_size = k, mse=mse, cpu_time_sample=cpu_time_sample, cpu_time_fit=cpu_time_fit)
  return(out)
}




#' Function to get Simple Random Sample
getSRS <- function(caseData, k){
  cpu_time_sample <- system.time({
  idx <- sample(caseData$N, k, T)
  X <- caseData$X[idx,]
  Y <- caseData$Y[idx]
  })
  
  cpu_time_fit <- system.time({
  eig <- eigen(t(X) %*% X, symmetric = T)
  #iI <- eig$vectors %*% (t(eig$vectors)/eig$values)
  beta <- eig$vectors %*% (t(eig$vectors)/eig$values) %*% t(X) %*% Y
  })
  
  mse <- rep(0, 2)
  mse[1] <- (beta[1] - caseData$beta[1])^2
  mse[2] <- sum((beta[-1]-caseData$beta[-1])^2)
  
  
  out <- list(X=X, Y=Y, beta=beta, betaTrue = caseData$beta, sample_size = k, mse=mse, cpu_time_sample=cpu_time_sample, cpu_time_fit=cpu_time_fit)
  return(out)
}




