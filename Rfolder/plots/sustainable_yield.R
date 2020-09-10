#' Sustainable Yield Plot
#' @param samples (data.frame) output of clean_chains.
#' @param file_path (character) file path to store the output jpeg file.
#' @param alpha (numeric) desired significance level for credible intervals.
#' @param leg_pos (numeric) vector of length 2 specifing the x and y coordinate where the legend should be positioned.
#' @param width (numeric) width of the output plot (in px).
#' @param height (numeric) height of the output plot (in px).
#' @export
sy_plot <- function(samples, model, file_path, alpha=0.50, leg_pos=c(17500, 40000), width=750, height=750, y_up=70000){
  
  if (is.element(model, c("base", "base_tvm", "base_ar", "base_tvm_ar"))){
    jpeg(file_path, width=width, height=height, unit="px")
    sy_c <- samples[,substr(names(samples), 1, 2)=="SY" &
                      substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "1"]
    sy_s <- samples[,substr(names(samples), 1, 2)=="SY" &
                      substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "2"]
    S <- (1:450)*50
    R_med_c <- apply(sy_c, MARGIN=2, quantile, prob=0.50)
    R_med_s <- apply(sy_s, MARGIN=2, quantile, prob=0.50)
    R_lo_c <- apply(sy_c, MARGIN=2, quantile, prob=alpha/2)
    R_hi_c <- apply(sy_c, MARGIN=2, quantile, prob=1-alpha/2)
    R_lo_s <- apply(sy_s, MARGIN=2, quantile, prob=alpha/2)
    R_hi_s <- apply(sy_s, MARGIN=2, quantile, prob=1-alpha/2)
    par(mfrow=c(1,1))
    par(mar=c(5,5,5,5))
    plot(S, R_hi_s, ty="l", main="Expected Yield", xlab="Number of Spawners", ylab="Expected Yield", ylim = c(0, y_up), font.lab=2, cex.main=2, cex.lab=1.5)
    s_df_hi <- data.frame(S, R_hi_c)
    s_df_hi <- s_df_hi[s_df_hi[,2]>0,]
    s_df_lo <- data.frame(S, R_lo_c)
    s_df_lo <- s_df_lo[s_df_lo[,2]>0,]
    shade_x <- c(s_df_hi[,1], s_df_hi[nrow(s_df_hi),1], s_df_lo[nrow(s_df_lo),1], s_df_lo[nrow(s_df_lo):1,1])
    shade_y <- c(s_df_hi[,2], c(0,0), s_df_lo[nrow(s_df_lo):1,2])
    polygon(x=shade_x, y=shade_y, col=rgb(1,0,0,0.3))
    s_df_hi <- data.frame(S, R_hi_s)
    s_df_hi <- s_df_hi[s_df_hi[,2]>0,]
    s_df_lo <- data.frame(S, R_lo_s)
    s_df_lo <- s_df_lo[s_df_lo[,2]>0,]
    shade_x <- c(s_df_hi[,1], s_df_hi[nrow(s_df_hi),1], s_df_lo[nrow(s_df_lo),1], s_df_lo[nrow(s_df_lo):1,1])
    shade_y <- c(s_df_hi[,2], c(0,0), s_df_lo[nrow(s_df_lo):1,2])
    polygon(x=shade_x, y=shade_y, col=rgb(0,0,1,0.3))
    lines(S[R_med_c>0], R_med_c[R_med_c>0], lwd=5, col=rgb(1,0,0,1))
    lines(S[R_lo_c>0], R_lo_c[R_lo_c>0], lwd=2)
    lines(S[R_hi_c>0], R_hi_c[R_hi_c>0], lwd=2)
    lines(S[R_med_s>0], R_med_s[R_med_s>0], lwd=5, col=rgb(0,0,1,1))
    lines(S[R_lo_s>0], R_lo_s[R_lo_s>0], lwd=2)
    lines(S[R_hi_s>0], R_hi_s[R_hi_s>0], lwd=2)
    blue_l_x <- c(leg_pos[1], leg_pos[1]+1000)
    blue_l_y <- c(leg_pos[2], leg_pos[2])
    lines(blue_l_x, blue_l_y, col=rgb(0,0,1,1), lwd=5)
    red_l_x <- c(leg_pos[1], leg_pos[1]+1000)
    red_l_y <- c(leg_pos[2]-1250, leg_pos[2]-1250)
    lines(red_l_x, red_l_y, col=rgb(1,0,0,1), lwd=5)
    dely <- 400
    delx <- 250
    blue_p_x <- c(leg_pos[1]+delx, leg_pos[1]+3*delx, leg_pos[1]+3*delx, leg_pos[1]+delx) 
    blue_p_y <- c(leg_pos[2]-2500+dely, leg_pos[2]-2500+dely, leg_pos[2]-2500-dely, leg_pos[2]-2500-dely)
    polygon(blue_p_x, blue_p_y, col=rgb(0,0,1,0.3))
    red_p_x <- c(leg_pos[1]+delx, leg_pos[1]+3*delx, leg_pos[1]+3*delx, leg_pos[1]+delx) 
    red_p_y <- c(leg_pos[2]-3750+dely, leg_pos[2]-3750+dely, leg_pos[2]-3750-dely, leg_pos[2]-3750-dely)
    polygon(red_p_x, red_p_y, col=rgb(1,0,0,0.3))
    text(leg_pos[1]+1000, leg_pos[2], "Posterior Median Salcha", pos=4)
    text(leg_pos[1]+1000, leg_pos[2]-1250, "Posterior Median Chena", pos=4)
    text(leg_pos[1]+1000, leg_pos[2]-2500, "50% CI Salcha", pos=4)
    text(leg_pos[1]+1000, leg_pos[2]-3750, "50% CI Salcha", pos=4)
    dev.off()
  }
  if (is.element(model, c("base_tvp"))){
    jpeg(file_path, width=width, height=height, unit="px")
    
    sy_c <- samples[,substr(names(samples), 1, 2)=="SY" &
                      substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "1"]
    sy_s <- samples[,substr(names(samples), 1, 2)=="SY" &
                      substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1) == "2"]
    S <- (1:450)*50
    R_med_c <- apply(sy_c, MARGIN=2, quantile, prob=0.50)
    R_med_s <- apply(sy_s, MARGIN=2, quantile, prob=0.50)
    
    n_years <- length(1986:2030)
    R_med_c_mat <- R_med_s_mat <- R_lo_c_mat <- R_hi_c_mat <- R_lo_s_mat <- R_hi_s_mat <- matrix(NA, nrow=450,  ncol=n_years)
    l <- 1
    for (j in 1:n_years){
      for (i in 1:450){
        R_med_c_mat[i, j] <- R_med_c[l]
        R_med_s_mat[i, j] <- R_med_s[l] 
        R_lo_c_mat[i, j] <- R_lo_c[l]
        R_hi_c_mat[i, j] <- R_hi_c[l]
        R_lo_s_mat[i, j] <- R_lo_s[l]
        R_hi_s_mat[i, j] <- R_hi_s[l]
        l <- l+1
      }
    }
    
    colnames(R_med_c_mat) <- colnames(R_med_s_mat) <- colnames(R_lo_c_mat) <- colnames(R_hi_c_mat) <- colnames(R_lo_s_mat) <- colnames(R_hi_s_mat) <- 1986:2030
    
    R_med_c_mat <- R_med_c_mat[,c(5,15,25,35,45)] 
    R_med_s_mat <- R_med_s_mat[,c(5,15,25,35,45)] 
    R_lo_c_mat <- R_lo_c_mat[,c(5,15,25,35,45)]
    R_hi_c_mat <- R_hi_c_mat[,c(5,15,25,35,45)]
    R_lo_s_mat <- R_lo_s_mat[,c(5,15,25,35,45)]
    R_hi_s_mat <- R_hi_s_mat[,c(5,15,25,35,45)]
    
    par(mfrow=c(1,2))
    
    R_med_c_90 <- R_med_c_mat[R_med_c_mat[,1]>0,1]
    S_med_c_90 <- S[1:length(R_med_c_90)]
    R_med_s_90 <- R_med_s_mat[R_med_s_mat[,1]>0,1]
    S_med_s_90 <- S[1:length(R_med_s_90)]
    
    R_med_c_00 <- R_med_c_mat[R_med_c_mat[,2]>0,2]
    S_med_c_00 <- S[1:length(R_med_c_00)]
    R_med_s_00 <- R_med_s_mat[R_med_s_mat[,2]>0,2]
    S_med_s_00 <- S[1:length(R_med_s_00)]
    
    R_med_c_10 <- R_med_c_mat[R_med_c_mat[,3]>0,3]
    S_med_c_10 <- S[1:length(R_med_c_10)]
    R_med_s_10 <- R_med_s_mat[R_med_s_mat[,3]>0,3]
    S_med_s_10 <- S[1:length(R_med_s_10)]
    
    R_med_c_20 <- R_med_c_mat[R_med_c_mat[,4]>0,4]
    S_med_c_20 <- S[1:length(R_med_c_20)]
    R_med_s_20 <- R_med_s_mat[R_med_s_mat[,4]>0,4]
    S_med_s_20 <- S[1:length(R_med_s_20)]
    
    R_med_c_30 <- R_med_c_mat[R_med_c_mat[,5]>0,5]
    S_med_c_30 <- S[1:length(R_med_c_30)]
    R_med_s_30 <- R_med_s_mat[R_med_s_mat[,5]>0,5]
    S_med_s_30 <- S[1:length(R_med_s_30)]
    
    S_msy_c_90 <- S_med_c_90[which(R_med_c_90==max(R_med_c_90))]
    R_msy_c_90 <- R_med_c_90[which(R_med_c_90==max(R_med_c_90))]
    
    S_msy_c_00 <- S_med_c_00[which(R_med_c_00==max(R_med_c_00))]
    R_msy_c_00 <- R_med_c_00[which(R_med_c_00==max(R_med_c_00))]
    
    S_msy_c_10 <- S_med_c_10[which(R_med_c_10==max(R_med_c_10))]
    R_msy_c_10 <- R_med_c_10[which(R_med_c_10==max(R_med_c_10))]
    
    S_msy_c_20 <- S_med_c_20[which(R_med_c_20==max(R_med_c_20))]
    R_msy_c_20 <- R_med_c_20[which(R_med_c_20==max(R_med_c_20))]
    
    S_msy_c_30 <- S_med_c_30[which(R_med_c_30==max(R_med_c_30))]
    R_msy_c_30 <- R_med_c_30[which(R_med_c_30==max(R_med_c_30))]
    
    S_msy_s_90 <- S_med_s_90[which(R_med_s_90==max(R_med_s_90))]
    R_msy_s_90 <- R_med_s_90[which(R_med_s_90==max(R_med_s_90))]
    
    S_msy_s_00 <- S_med_s_00[which(R_med_s_00==max(R_med_s_00))]
    R_msy_s_00 <- R_med_s_00[which(R_med_s_00==max(R_med_s_00))]
    
    S_msy_s_10 <- S_med_s_10[which(R_med_s_10==max(R_med_s_10))]
    R_msy_s_10 <- R_med_s_10[which(R_med_s_10==max(R_med_s_10))]
    
    S_msy_s_20 <- S_med_s_20[which(R_med_s_20==max(R_med_s_20))]
    R_msy_s_20 <- R_med_s_20[which(R_med_s_20==max(R_med_s_20))]
    
    S_msy_s_30 <- S_med_s_30[which(R_med_s_30==max(R_med_s_30))]
    R_msy_s_30 <- R_med_s_30[which(R_med_s_30==max(R_med_s_30))]
    
    plot(S_med_c_90, R_med_c_90, ty="l", main="Chena", xlab="Number of Spawners", ylab="Expected Yield", font.lab=2, cex.main=2,  cex.lab=1.5, col="red")
    lines(S_med_c_00, R_med_c_00, col="green")
    lines(S_med_c_10, R_med_c_10, col="blue")
    lines(S_med_c_20, R_med_c_20, col="orange")
    lines(S_med_c_30, R_med_c_30, col="purple")
    lines(c(0,20000), c(0,0))
    
    lines(c(S_msy_c_90, S_msy_c_90), c(0, R_msy_c_90), col="red")
    lines(c(S_msy_c_00, S_msy_c_00), c(0, R_msy_c_00), col="green")
    lines(c(S_msy_c_10, S_msy_c_10), c(0, R_msy_c_10), col="blue")
    lines(c(S_msy_c_20, S_msy_c_20), c(0, R_msy_c_20), col="orange")
    lines(c(S_msy_c_30, S_msy_c_30), c(0, R_msy_c_30), col="purple")
    
    plot(S_med_s_90, R_med_s_90, ty="l", main="Salcha", xlab="Number of Spawners", ylab="Expected Yield", font.lab=2, cex.main=2,  cex.lab=1.5, col="red")
    lines(S_med_s_00, R_med_s_00, col="green")
    lines(S_med_s_10, R_med_s_10, col="blue")
    lines(S_med_s_20, R_med_s_20, col="orange")
    lines(S_med_s_30, R_med_s_30, col="purple")
    lines(c(0,20000), c(0,0))
    
    lines(c(S_msy_s_90, S_msy_s_90), c(0, R_msy_s_90), col="red")
    lines(c(S_msy_s_00, S_msy_s_00), c(0, R_msy_s_00), col="green")
    lines(c(S_msy_s_10, S_msy_s_10), c(0, R_msy_s_10), col="blue")
    lines(c(S_msy_s_20, S_msy_s_20), c(0, R_msy_s_20), col="orange")
    lines(c(S_msy_s_30, S_msy_s_30), c(0, R_msy_s_30), col="purple")
    
    dev.off()
  }
  
}
