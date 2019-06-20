#' Customized Simulation Functions, returns MSE and plots
#' 
#' @export
demoIboss <- function(case=NULL, n=NULL, k=NULL, p=NULL, rept=NULL, compare=F){
  if(is.null(case)){
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
  }
  if (is.null(n)){ n <- c(5000, 10^4, 10^5, 10^6) }
  if (is.null(k)){ k <- c(1000)}
  if (is.null(p)){ p <- c(50)} 
  if (is.null(rep)){ rept = c(100)}
  submethods <- c("FULL", "UNI", "LEV", "D-OPT")
  
  # if compare is desired, output a matrix of k and n with mses ----
  if (compare){
  # if k is a vector and n is a scalar ----
    # mse matrix: colnames=k, rownames = methods
  if(length(k) > 1 & length(n) == 1){
    cat("k is a vector, n is a scalar, comparison is desired\n")
  mse0 <- mse1 <- cpu_times <- matrix(0, ncol= length(k), nrow = length(submethods))
  colnames(mse0) <- colnames(mse1) <- colnames(cpu_times) <-paste0("k=",k)
  rownames(mse0) <- rownames(mse1) <- rownames(cpu_times) <- submethods
  
    for (j in 1:length(k)){
      tmp <- getMSE(case, n, k[j], p, rept = rept, compare=compare)
      # full data method, first row ----
      mse0[1,j] <- tmp$mse0_all
      mse1[1,j] <- tmp$mse1_all
      cpu_times[1,j] <- tmp$avg_cpu_time_simulation
      
      # UNI method, second row ----
      mse0[2,j] <- tmp$mse0_srs
      mse1[2,j] <- tmp$mse1_srs
      cpu_times[2,j] <- tmp$avg_cpu_time_srs
      
      # LEVERAGE method, third row ----
      mse0[3,j] <- tmp$mse0_lev
      mse1[3,j] <- tmp$mse1_lev
      cpu_times[3,j] <- tmp$avg_cpu_time_lev
      
      # IOBSS method, forth row ----
      mse0[4,j] <- tmp$mse0_iboss
      mse1[4,j] <- tmp$mse1_iboss
      cpu_times[4,j] <- tmp$avg_cpu_time_iboss
    }
  
  out <- list(mse0=mse0, mse1=mse1, cpu_time=cpu_times)
  
  # reshape mse0 and mse1 for plot ----
  mse1 <- data.frame(mses = c(log10(mse1)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(k)), logN = rep(k, each = length(k)))
  mse0 <- data.frame(mses = c(log10(mse0)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(k)), logN = rep(k, each = length(k))) 
  
  
  mse1_plot <- ggplot2::ggplot(data=mse1, ggplot2::aes(x=logN, y = mses, colour=factor(mse1$methods), shape=factor(mse1$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse1$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(k), max(k))) + ggplot2::xlab("Subsample Size") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'),axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Slopes")
  
  mse0_plot <- ggplot2::ggplot(data=mse0, ggplot2::aes(x=logN, y = mses, colour=factor(mse0$methods), shape=factor(mse0$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse0$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(k), max(k))) + ggplot2::xlab("Subsample Size") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'), axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Intercepts")
  
  out$mse0_plot <- mse0_plot
  out$mse1_plot <- mse1_plot
  }
  # end of k is vector and n is scalar
  # --------------------------------- #
  
  # if k is a scalar and n is vector #
  if (length(n) > 1 & length(k) == 1){
    cat("n is a vector, k is a scalar, comparison is desired\n")
    mse0 <- mse1 <- cpu_times <- matrix(0, ncol= length(n), nrow = length(submethods))
    colnames(mse0) <- colnames(mse1) <- colnames(cpu_times) <-paste0("n=",n)
    rownames(mse0) <- rownames(mse1) <- rownames(cpu_times) <- submethods
    
    for (j in 1:length(n)){
      tmp <- getMSE(case, n[j], k, p, rept=rept, compare=compare)
      # full data method, first row ----
      mse0[1,j] <- tmp$mse0_all
      mse1[1,j] <- tmp$mse1_all
      cpu_times[1,j] <- tmp$avg_cpu_time_simulation
      
      # UNI method, second row ----
      mse0[2,j] <- tmp$mse0_srs
      mse1[2,j] <- tmp$mse1_srs
      cpu_times[2,j] <- tmp$avg_cpu_time_srs
      
      # LEVERAGE method, third row ----
      mse0[3,j] <- tmp$mse0_lev
      mse1[3,j] <- tmp$mse1_lev
      cpu_times[3,j] <- tmp$avg_cpu_time_lev
      
      # IOBSS method, forth row ----
      mse0[4,j] <- tmp$mse0_iboss
      mse1[4,j] <- tmp$mse1_iboss
      cpu_times[4,j] <- tmp$avg_cpu_time_iboss
    }
    
    out <- list(mse0=mse0, mse1=mse1, cpu_time=cpu_times)
    # reshape mse0 and mse1 for plot ----
    mse1 <- data.frame(mses = c(log10(mse1)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(n)), logN = rep(log10(n), each=4))
    mse0 <- data.frame(mses = c(log10(mse0)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(n)), logN = rep(log10(n),each = 4)) 
    
    
    mse1_plot <- ggplot2::ggplot(data=mse1, ggplot2::aes(x=logN, y = mses, colour=factor(mse1$methods), shape=factor(mse1$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse1$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(log10(n)), max(log10(n)))) + ggplot2::xlab("log10(Full Data Size)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'),axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=15,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Slopes")
    
    mse0_plot <- ggplot2::ggplot(data=mse0, ggplot2::aes(x=logN, y = mses, colour=factor(mse0$methods), shape=factor(mse0$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse0$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(log10(n)), max(log10(n)))) + ggplot2::xlab("log10(Full Data Size)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'), axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=15,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Intercepts")
    
    out$mse0_plot <- mse0_plot
    out$mse1_plot <- mse1_plot
  }
  # end of n is vector and k is scalar
  # --------------------------------- #
  # end of comparison is desired.
  }else{
    cat("no restrictions on n,k, comparison is not desired\n")
  # compare is not desired: ----
  # allows n, p to be vectors ----
  # returns mse0, mse1, and cpu_time matrices, no plots
    mse0 <- mse1 <- cpu_times <- matrix(0, ncol= length(k), nrow = length(n))
    colnames(mse0) <- colnames(mse1) <- colnames(cpu_times) <- paste0("k=", k)
    rownames(mse0) <- rownames(mse1) <- rownames(cpu_times) <- paste0("n=", n)
      
  for (i in 1:length(n)){
    for(j in 1:length(k)){
      tmp <- getMSE(case, n[i], k[j], p, rept=rept, compare=F)
      mse0[i,j] <- tmp$mse0_iboss
      mse1[i,j] <- tmp$mse1_iboss
      cpu_times[i,j] <- tmp$avg_cpu_time_iboss
    }
  }
  
  out <- list(mse0=mse0, mse1=mse1, cpu_time=cpu_times, mse0_plot=NULL, mse1_plot=NULL)  
  }  
  
  return(out)  
}




















demoFigures <- function(figNum=NULL, p=NULL, rep=NULL){
  figs <- c("Comparisons of three subsampling methods in terms of MSEs\n with fixed subsample size 1000\n and ranging size of full data\n n=5000, 10^4, 10^5, 10^6\n",
            "Comparisons of three subsampling methods in terms of MSEs\n with fixed size of full data 10^6\n and ranging size of subsample size\n k = 200, 400, 500, 1000, 2000, 3000, 5000\n",
            "Comparisons of IBOSS and Full Data in terms of statistical inference\nPlots of coverage probabilities and average length of 95% confidence intervals\n against ranging size of full data\nn=5000, 10^4, 10^5, 10^6\nfixed subsample size k = 1000\n",
            "Food Intakes Data\n",
            "Chemical Sensor Data\n")
  if (is.null(figNum)) {
    figNum <- menu(figs)
  }
  
  cat("CASE SELECTED", figNum, "\n") 
  if (is.null(p)){
    p <- 50
  }
  
  
  if (is.null(rep)){
    rep = 1000
  }
  
  switch(figNum, {
    # figure 1&2, comparisons of intercept and slopes
    N <- c(5000, 10^4, 10^5, 10^6)
    k <- 1000
    case <- 1:5
    p <- 50
    
    for (i in case) {
      # mse matrices: each row is a case, each col is different N
      mse0 <- mse1 <- matrix(0, nrow = 4, ncol = length(N))
      for (Ni in 1:length(N)) {
        tmp <- getMSE(i, N[Ni], k, p, rep = rep, compare = T)
        mse0[1,Ni] <- tmp$mse0_all
        mse0[2,Ni] <- tmp$mse0_srs
        mse0[3,Ni] <- tmp$mse0_lev
        mse0[4,Ni] <- tmp$mse0_iboss
        mse1[1,Ni] <- tmp$mse1_all
        mse1[2,Ni] <- tmp$mse1_srs
        mse1[3,Ni] <- tmp$mse1_lev
        mse1[4,Ni] <- tmp$mse1_iboss
      }
      
    mse1 <- data.frame(mses = c(log10(mse1)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(N)), logN = rep(log10(N), each = length(N)))
    mse0 <- data.frame(mses = c(log10(mse0)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(N)), logN = rep(log10(N),each = length(N))) 
      
    
    mse1_plot <- ggplot2::ggplot(data=mse1, ggplot2::aes(x=logN, y = mses, colour=factor(mse1$methods), shape=factor(mse1$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse1$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(log10(N)), max(log10(N)))) + ggplot2::xlab("log10(N)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'),axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Slopes")

    mse0_plot <- ggplot2::ggplot(data=mse0, ggplot2::aes(x=logN, y = mses, colour=factor(mse0$methods), shape=factor(mse0$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse0$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(log10(N)), max(log10(N)))) + ggplot2::xlab("log10(N)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'), axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Intercepts")
    
    plot(mse1_plot)
    plot(mse0_plot)
    out <- list(slope_plot=mse1_plot, inter_plot=mse0_plot)
    rm(mse1_plot)
    rm(mse0_plot)
    }
  }, 
  # figure 3, mse of beta1 vs changing k
  {
    N = 10^6
    case <- 1:6
    kk <- c(200, 400, 500, 1000, 2000, 3000, 5000)
    for (i in case) {
      # mse matrices: each row is a case, each col is different N
      mse0 <- mse1 <- matrix(0, nrow = 4, ncol = length(kk))
      for (Ki in 1:length(kk)) {
        tmp <- getMSE(i, N, Ki, p, rep = rep, compare = T)
        mse0[1,Ki] <- tmp$mse0_all
        mse0[2,Ki] <- tmp$mse0_srs
        mse0[3,Ki] <- tmp$mse0_lev
        mse0[4,Ki] <- tmp$mse0_iboss
        mse1[1,Ki] <- tmp$mse1_all
        mse1[2,Ki] <- tmp$mse1_srs
        mse1[3,Ki] <- tmp$mse1_lev
        mse1[4,Ki] <- tmp$mse1_iboss
      }
      
      mse1 <- data.frame(mses = c(log10(mse1)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(kk)), logN = rep(kk, each = length(kk)))
      mse0 <- data.frame(mses = c(log10(mse0)), methods = rep(c("FULL", "UNI", "LEV", "D-OPT"),length(kk)), logN = rep(kk,each = length(kk))) 
      
      
      mse1_plot <- ggplot2::ggplot(data=mse1, ggplot2::aes(x=logN, y = mses, colour=factor(mse1$methods), shape=factor(mse1$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse1$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(kk), max(kk))) + ggplot2::xlab("Subsample Size (K)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'),axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Slopes")
      
      mse0_plot <- ggplot2::ggplot(data=mse0, ggplot2::aes(x=logN, y = mses, colour=factor(mse0$methods), shape=factor(mse0$methods))) + ggplot2::geom_line(ggplot2::aes(group=factor(mse0$methods))) + ggplot2::geom_point(size = 3) + ggplot2::xlim(c(min(kk), max(kk))) + ggplot2::xlab("Subsample Size (K)") + ggplot2::ylab("log10(MSE)") + ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = 'bottom', plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = 'bold'), axis.title.y = ggplot2::element_text(size=20,face="bold"),axis.text.x = ggplot2::element_text(size=20,face="bold"),axis.text.y = ggplot2::element_text(size=20,face="bold"),axis.title.x = ggplot2::element_text(size=20,face="bold")) + ggplot2::ggtitle("MSE of Intercepts")
      
      plot(mse1_plot)
      plot(mse0_plot)
      out <- list(slope_plot=mse1_plot, inter_plot=mse0_plot)
      rm(mse1_plot)
      rm(mse0_plot)
    }
  },
  {
    # coverage probability 
    # limited to case 1 & 4
    N <- c(5000, 10^4, 10^5, 10^6)
    k <- 1000
    ##################
    # finished in the future
    ##################
  },
  {
    # computing time in table 2
    
    
    # left half of table 2
    N <- c(5000, 50000, 500000)
    p <- 500
    k <- 1000
    cpu_time_1 <- matrix(0, nrow = length(N), ncol = 4)
    case <- 1
    for (i in 1:length(N)){
      tmp <- getMSE(case, N[i], k, p, rep = 2, compare = T)
      cpu_time_1[i,4] <- tmp$avg_cpu_time_simulation
      cpu_time_1[i,1] <- tmp$avg_cpu_time_iboss
      cpu_time_1[i,2] <- tmp$avg_cpu_time_srs
      cpu_time_1[i,3] <- tmp$avg_cpu_time_lev
    }
    rm(tmp)
    colnames(cpu_time_1) <- c(5000, 50000, 500000)
    rownames(cpu_time_1) <- c("D-OPT", "UNI", "LEV", "FULL")
    
    # right half of table 2
    N <- 500000
    p <- c(10, 100, 500)
    k <- 1000
    cpu_time_2 <- matrix(0, nrow = length(p), ncol = 4)
    for (i in 1:length(p)){
      tmp <- getMSE(case, N, k, p[i], rep = 2, compare = T)
      cpu_time_2[i, 4] <- tmp$avg_cpu_time_simulation
      cpu_time_2[i, 1] <- tmp$avg_cpu_time_iboss
      cpu_time_2[i, 2] <- tmp$avg_cpu_time_srs
      cpu_time_2[i, 3] <- tmp$avg_cpu_time_lev
    }
    
    colnames(cpu_time_2) <- c(10, 100, 500)
    rownames(cpu_time_2) <- c("D-OPT", "UNI", "LEV", "FULL")
    
    out <- list(ntable=cpu_time_1, ptable=cpu_time_2)
  })
  
  return(out)
  
}




