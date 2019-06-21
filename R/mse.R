#' calculating Mean Squared Errors (MSE)
#' @param case integer. Distribution to generate full data.
#' @param n integer. Full data size.
#' @param k integer. Subsample size.
#' @param p integer. Number of predictors.
#' @param rept integer. Repeat times, defualt is 100.
#' @param compare logical. If True, output comparisons to other subsampling methods; Otherwise, output iboss sampling results only.
#' @export
getMSE <- function(case, n, k, p, rept = NULL, compare = F){
  # caseData has to be a list consists of 
  # design matrix: X(intercept included)
  # response vector: Y 
  # true parameter: beta
  # total number of obs(possible) : N
  # case number: case
  # number of predictors: dim(beta) - 1
  if (is.null(rept)) {rept = 100}
  cat("Working on...case =", case, "n =", n, "k =",k, "p =", p, "rep =", rept, "compare=", compare, "...")
  if(compare){
  
  mse_all <- mse_iboss <- mse_srs <- mse_lev <- matrix(0, ncol=2, nrow=rept) # first col is mse0, second col is mse1
  cpu_time_iboss <- cpu_time_srs <- cpu_time_lev <- cpu_time_all <- matrix(0, ncol=2, nrow=rept) # first col is sampling time, second col is fitting time.
  for(i in 1:rept){
    cat("rept =" , i, "...")
    caseData <- Generate_Case_Full_Data(case, n, p)
    iboss_data <- getIBOSS(caseData, k)
    srs_data <- getSRS(caseData, k)
    lev_data <- getLEV(caseData, k)
    mse_all[i,] <- caseData$mse
    mse_iboss[i,] <- iboss_data$mse
    mse_srs[i,] <- srs_data$mse
    mse_lev[i,] <- lev_data$mse
    cpu_time_iboss[i,] <- c(iboss_data$cpu_time_sample[3], iboss_data$cpu_time_fit[3])
    cpu_time_lev[i,] <- c(lev_data$cpu_time_sample[3], lev_data$cpu_time_fit[3])
    cpu_time_srs[i,] <- c(srs_data$cpu_time_sample[3], srs_data$cpu_time_fit[3])
    cpu_time_all[i,] <- c(caseData$cpu_time_simulation[3], caseData$cpu_time_fit[3])
  }
  
  out <- list(mse0_all=mean(mse_all[,1]), mse1_all=mean(mse_all[,2]),
    mse0_iboss=mean(mse_iboss[,1]), mse1_iboss=mean(mse_iboss[,2]), 
              avg_cpu_time_iboss = mean(cpu_time_iboss[,1]),
              mse0_srs=mean(mse_srs[,1]), mse1_srs=mean(mse_srs[,2]), 
              avg_cpu_time_srs = mean(cpu_time_srs[,1]),
              mse0_lev=mean(mse_lev[,1]), mse1_lev=mean(mse_lev[,2]), 
              avg_cpu_time_lev = mean(cpu_time_lev[,1]), 
              avg_cpu_time_simulation=mean(cpu_time_all[,1])
              )
  }else{
    mse_iboss <- mse_all <- matrix(0, ncol=2, nrow=rept) # first col is mse0, second col is mse1
    cpu_time_iboss <- cpu_time_all <- matrix(0, ncol=2, nrow=rept) # first col is sampling time, second col is fitting time.
    for(i in 1:rept){
      cat("rept =" , i, "...")
      caseData <- Generate_Case_Full_Data(case, n, p)
      iboss_data <- getIBOSS(caseData, k)
      mse_all[i,] <- caseData$mse
      mse_iboss[i,] <- iboss_data$mse
      cpu_time_iboss[i,] <- c(iboss_data$cpu_time_sample[2], iboss_data$cpu_time_fit[2])
      cpu_time_all[i,] <- c(caseData$cpu_time_simulation[3], caseData$cpu_time_fit[3])
    }
    
    out <- list(mse0_all=mean(mse_all[,1]), mse1_all=mean(mse_all[,2]), 
      mse0_iboss=mean(mse_iboss[,1]), mse1_iboss=mean(mse_iboss[,2]), 
                avg_cpu_time_iboss = mean(cpu_time_iboss[,1]),
                avg_cpu_time_simulation=mean(cpu_time_all[,1]))
  }
  
  cat('done\n')
  
  return(out)
}




