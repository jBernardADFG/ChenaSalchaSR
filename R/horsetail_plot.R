#' Horsetail Plot
#' @param samples (data.frame) output of clean_chains
#' @param model (character) the model being run
#' @param file_path (character) file path to store output xlsx file
#' @param n_draws (numeric) the number of SR relations to be simulated
#' @param sig_lev (numeric) significance level for the error bars
#' @param width (numeric) width of the output plot (in px)
#' @param height (numeric) height of the output plot (in px)
#' @param s_up (numeric) vector of length 1 specifying the upper bounds for the x axis.
#' @param r_up (numeric) vector of length 2 specifying the upper bound for the y axis. The first entry cooresponds to the Chena while the second entry cooresponds to the Salcha.
#' @export
horsetail_plot <- function(samples, model, file_path, n_draws=25, sig_lev=0.50, width=800, height=800, s_up=20000, r_up=c(30000, 30000)){
  
  if (is.element(model, c("base_tvp", "base_tvp_ld"))){
    stop(paste("Function has not been set up for", model))
  }
  
  alpha_c <- samples[, names(samples)=="alpha[1]"]
  alpha_s <- samples[, names(samples)=="alpha[2]"]
  
  beta_c <- samples[, names(samples)=="beta[1]"]
  beta_s <- samples[, names(samples)=="beta[2]"]
  
  spawners_c <- samples[, substr(names(samples), 1, 2) == "S[" &
                          substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "1"]
  spawners_s <- samples[, substr(names(samples), 1, 2) == "S[" &
                          substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "2"]
  
  recruits_c <- samples[, substr(names(samples), 1, 2) == "R[" &
                          substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "1"]
  
  recruits_s <- samples[, substr(names(samples), 1, 2) == "R[" &
                          substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "2"]
  
  jpeg(file_path, width=width, height=height, unit="px")
  
  par(mfrow=c(2,1))
  
  plot(1, type="n", xlab="Number of Spawners", ylab="Number of Recruits", main="Chena River", xlim=c(0, s_up), ylim=c(0, r_up[1]), font.lab=2, cex.main=2, cex.lab=1.5)
  
  if (is.element(model, c("base_ld"))){
    
    S_crit_c <- samples[, names(samples)=="S_crit[1]"]
    S_crit_s <- samples[, names(samples)=="S_crit[1]"]
    
    jordy_sr <- function(S, S_crit, alpha, beta){
      R <- rep(NA, length(S))
      for (i in 1:length(S)){
        if (S[i] < S_crit){
          R[i] <- alpha/(2*S_crit)*S[i]^2
        }else{
          R[i] <- alpha*(S[i]-S_crit)*exp(-beta*(S[i]-S_crit))+alpha/2*S_crit
        }
      }
      return(R)
    }
    
    # Add y=x line
    S <- 1:s_up
    lines(S, S, lty=2, lwd=2)
    
    # Add median line to plot
    alpha_c_med <- median(alpha_c) 
    beta_c_med <- median(beta_c)
    S_crit_c_med <- median(S_crit_c)
    
    R <- jordy_sr(S, S_crit_c_med, alpha_c_med, beta_c_med)
    lines(S, R, lwd=4, col=rgb(0,0,1,1))
    
    # Draw alpha and beta and add lines
    for (i in 1:n_draws){
      alpha <- sample(alpha_c, 1)
      beta <- sample(beta_c, 1)
      S_crit <- sample(S_crit_c, 1)
      
      R <- jordy_sr(S, S_crit, alpha, beta)
      lines(S, R, lwd=1, col=rgb(0,0,1,0.3))
    }
    
    labs <- substr(as.character(1986:2020), 3, 4)
    
    # Add Estimates of the Number of Spawners and the number of Recruits
    S_est <- apply(spawners_c, MARGIN=2, median)
    R_est <- apply(recruits_c, MARGIN=2, median)
    points(S_est, R_est, col=rgb(1,0,0,1), cex=1.5, pch=19)
    
    # pos <- rep(NULL, length(labs)) 
    # pos[which(labs=="01")] <- 2
    # pos[which(labs=="04")] <- 4
    # for(i in 1:length(labs)){
    #   if (is.na(pos[i])){
    #     print(text(R_est[i]~S_est[i], labels = labs[i], cex=1, font=4))
    #   }
    #   else{
    #     print(text(R_est[i]~S_est[i], labels = labs[i], pos=pos[i], cex=1, font=4))
    #   }
    # }
    
    
    # Add Error Bars to the Estimates
    S_lo <- apply(spawners_c, MARGIN=2, quantile, probs=sig_lev/2)
    S_hi <- apply(spawners_c, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    R_lo <- apply(recruits_c, MARGIN=2, quantile, probs=sig_lev/2)
    R_hi <- apply(recruits_c, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    for (i in 1:length(S_lo)){
      lines(c(S_lo[i], S_hi[i]), c(R_est[i], R_est[i]), col=rgb(1,0,0,1), lwd=2)
      lines(c(S_est[i], S_est[i]), c(R_lo[i], R_hi[i]), col=rgb(1,0,0,1), lwd=2)
    }
    
    plot(1, type="n", xlab="Number of Spawners", ylab="Number of Recruits", main="Salcha River", xlim=c(0, s_up), ylim=c(0, r_up[2]), font.lab=2, cex.main=2, cex.lab=1.5)
    
    
    # Add y=x line
    lines(S, S, lty=2, lwd=2)
    
    # Add median line to plot
    alpha_s_med <- median(alpha_s) 
    beta_s_med <- median(beta_s)
    S_crit_s_med <- median(S_crit_s)
    
    R <- jordy_sr(S, S_crit_s_med, alpha_s_med, beta_s_med)
    lines(S, R, lwd=4, col=rgb(0,0,1,1))
    
    # Draw alpha and beta and add lines
    for (i in 1:n_draws){
      alpha <- sample(alpha_s, 1)
      beta <- sample(beta_s, 1)
      S_crit <- sample(S_crit, 1)
      
      R <- jordy_sr(S, S_crit, alpha, beta)
      lines(S, R, lwd=1, col=rgb(0,0,1,0.3))
    }
    
    # Add Estimates of the Number of Spawners and the number of Recruits
    S_est <- apply(spawners_s, MARGIN=2, median)
    R_est <- apply(recruits_s, MARGIN=2, median)
    points(S_est, R_est, col=rgb(1,0,0,1), cex=1.5, pch=19)
    
    pos <- rep(NULL, length(labs))
    pos[which(labs=="20")] <- 2
    pos[which(labs=="05")] <- 4
    pos[which(labs=="96")] <- 2
    pos[which(labs=="15")] <- 4
    # for(i in 1:length(labs)){
    #   if (is.na(pos[i])){
    #     print(text(R_est[i]~S_est[i], labels = labs[i], cex=1, font=4))
    #   }
    #   else{
    #     print(text(R_est[i]~S_est[i], labels = labs[i], pos=pos[i], cex=1, font=4))
    #   }
    # }
    
    # Add Error Bars to the Estimates
    S_lo <- apply(spawners_s, MARGIN=2, quantile, probs=sig_lev/2)
    S_hi <- apply(spawners_s, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    R_lo <- apply(recruits_s, MARGIN=2, quantile, probs=sig_lev/2)
    R_hi <- apply(recruits_s, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    for (i in 1:length(S_lo)){
      lines(c(S_lo[i], min(S_hi[i], s_up)), c(R_est[i], R_est[i]), col=rgb(1,0,0,1), lwd=2)
      lines(c(S_est[i], S_est[i]), c(R_lo[i], R_hi[i]), col=rgb(1,0,0,1), lwd=2)
    }
    
    dev.off()
    
    
    
  }else{
    rs <- function(S, alpha, beta){
      alpha*S*exp(-beta*S)
    }
    # Add y=x line
    S <- 1:s_up
    lines(S, S, lty=2, lwd=2)
    
    # Add median line to plot
    alpha_c_med <- median(alpha_c) 
    beta_c_med <- median(beta_c)
    R <- rs(S, alpha_c_med, beta_c_med)
    lines(S, R, lwd=4, col=rgb(0,0,1,1))
    
    # Draw alpha and beta and add lines
    for (i in 1:n_draws){
      alpha <- sample(alpha_c, 1)
      beta <- sample(beta_c, 1)
      R <- rs(S, alpha, beta)
      lines(S, R, lwd=1, col=rgb(0,0,1,0.3))
    }
    
    labs <- substr(as.character(1986:2020), 3, 4)
    
    # Add Estimates of the Number of Spawners and the number of Recruits
    S_est <- apply(spawners_c, MARGIN=2, median)
    R_est <- apply(recruits_c, MARGIN=2, median)
    points(S_est, R_est, col=rgb(1,0,0,1), cex=1.5, pch=19)
    
    # pos <- rep(NULL, length(labs)) 
    # pos[which(labs=="01")] <- 2
    # pos[which(labs=="04")] <- 4
    # for(i in 1:length(labs)){
    #   if (is.na(pos[i])){
    #     print(text(R_est[i]~S_est[i], labels = labs[i], cex=1, font=4))
    #   }
    #   else{
    #     print(text(R_est[i]~S_est[i], labels = labs[i], pos=pos[i], cex=1, font=4))
    #   }
    # }
    
    
    # Add Error Bars to the Estimates
    S_lo <- apply(spawners_c, MARGIN=2, quantile, probs=sig_lev/2)
    S_hi <- apply(spawners_c, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    R_lo <- apply(recruits_c, MARGIN=2, quantile, probs=sig_lev/2)
    R_hi <- apply(recruits_c, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    for (i in 1:length(S_lo)){
      lines(c(S_lo[i], S_hi[i]), c(R_est[i], R_est[i]), col=rgb(1,0,0,1), lwd=2)
      lines(c(S_est[i], S_est[i]), c(R_lo[i], R_hi[i]), col=rgb(1,0,0,1), lwd=2)
    }
    
    plot(1, type="n", xlab="Number of Spawners", ylab="Number of Recruits", main="Salcha River", xlim=c(0, s_up), ylim=c(0, r_up[2]), font.lab=2, cex.main=2, cex.lab=1.5)
    
    rs <- function(S, alpha, beta){
      alpha*S*exp(-beta*S)
    }
    
    # Add y=x line
    lines(S, S, lty=2, lwd=2)
    
    # Add median line to plot
    alpha_s_med <- median(alpha_s) 
    beta_s_med <- median(beta_s)
    
    R <- rs(S, alpha_s_med, beta_s_med)
    lines(S, R, lwd=4, col=rgb(0,0,1,1))
    
    # Draw alpha and beta and add lines
    for (i in 1:n_draws){
      alpha <- sample(alpha_s, 1)
      beta <- sample(beta_s, 1)
      R <- rs(S, alpha, beta)
      lines(S, R, lwd=1, col=rgb(0,0,1,0.3))
    }
    
    # Add Estimates of the Number of Spawners and the number of Recruits
    S_est <- apply(spawners_s, MARGIN=2, median)
    R_est <- apply(recruits_s, MARGIN=2, median)
    points(S_est, R_est, col=rgb(1,0,0,1), cex=1.5, pch=19)
    
    pos <- rep(NULL, length(labs))
    pos[which(labs=="20")] <- 2
    pos[which(labs=="05")] <- 4
    pos[which(labs=="96")] <- 2
    pos[which(labs=="15")] <- 4
    # for(i in 1:length(labs)){
    #   if (is.na(pos[i])){
    #     print(text(R_est[i]~S_est[i], labels = labs[i], cex=1, font=4))
    #   }
    #   else{
    #     print(text(R_est[i]~S_est[i], labels = labs[i], pos=pos[i], cex=1, font=4))
    #   }
    # }
    
    # Add Error Bars to the Estimates
    S_lo <- apply(spawners_s, MARGIN=2, quantile, probs=sig_lev/2)
    S_hi <- apply(spawners_s, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    R_lo <- apply(recruits_s, MARGIN=2, quantile, probs=sig_lev/2)
    R_hi <- apply(recruits_s, MARGIN=2, quantile, probs=1-sig_lev/2)
    
    for (i in 1:length(S_lo)){
      lines(c(S_lo[i], min(S_hi[i], s_up)), c(R_est[i], R_est[i]), col=rgb(1,0,0,1), lwd=2)
      lines(c(S_est[i], S_est[i]), c(R_lo[i], R_hi[i]), col=rgb(1,0,0,1), lwd=2)
    }

    dev.off()
  }
}
