#' Sustainable Yield Plot
#' @param samples (data.frame) output of clean_chains.
#' @param file_path (character) file path to store the output jpeg file.
#' @param alpha (numeric) desired significance level for credible intervals.
#' @param leg_pos (numeric) vector of length 2 specifing the x and y coordinate where the legend should be positioned.
#' @param width (numeric) width of the output plot (in px).
#' @param height (numeric) height of the output plot (in px).
#' @export
sy_plot <- function(samples, file_path, alpha=0.50, leg_pos=c(17500, 40000), width=750, height=750, y_up=70000){
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
