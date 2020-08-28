#' Optimal Yield, Overfishing, and Optimal Recruitment Probability Plot
#' @param samples (data.frame) output of clean_chains.
#' @param file_path (character) file path to store the output jpeg file.
#' @param old_goal_median (numeric) vector of length 2 specifying the old escapement goal. The first entry cooresponds to the Chena while the second entry cooresponds to the Salcha.  
#' @param old_goal_lo (numeric) vector of length 2 specifying the lower bound of the old escapement goal. The first entry cooresponds to the Chena while the second entry cooresponds to the Salcha. 
#' @param old_goal_up (numeric)  vector of length 2 specifying the upper bound of the old escapement goal. The first entry cooresponds to the Chena while the second entry cooresponds to the Salcha. 
#' @param legend_position (numeric) vector of length 2 specifying the x and y coordinate where the legend should be positioned in each plot.
#' @param width (numeric) the width of the output jpeg file (in px).
#' @param height (numeric) the height of the output jpeg file (in px).
#' @export
profile_plot <- function(samples, file_path, old_goal_med, old_goal_lo, old_goal_up, legend_position = c(15000, 1), width=1000, height=1000){
  jpeg(filename=file_path, width=width, height=height, units="px")
  layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7), ncol=2, byrow=TRUE), heights=c(4,4,4, 2))
  S <- (1:450)*50
  # ----------------------------------
  # OPTIMAL YIELD PROBABILITY PROFILES
  # FOR THE CHENA
  c_70 <- samples[,substr(names(samples),1,6)=="I_70_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  c_80 <- samples[,substr(names(samples),1,6)=="I_80_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  c_90 <- samples[,substr(names(samples),1,6)=="I_90_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  temp <- apply(c_70, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Achieving...", main="Optimal Yield -- Chena River", lwd=3, ylim=c(0,1))
  temp <- apply(c_80, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(c_90, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[1], old_goal_lo[1], old_goal_up[1], old_goal_up[1])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[1], old_goal_med[1]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSY", cex=1, pos=4)
  # poly_x <- c(5500, 5500, 6000, 6000)
  # poly_y <- c(0, 0.05, 0.05, 0)
  # polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  # lines(c(5500, 6000), c(0.05/2, 0.05/2), lty=2, lwd=3)
  # text(x=6000,  y=0.05/2, labels="Old Escapement Goal", cex=1, pos=4)
    
  # FOR THE SALCHA
  s_70 <- samples[,substr(names(samples),1,6)=="I_70_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  s_80 <- samples[,substr(names(samples),1,6)=="I_80_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  s_90 <- samples[,substr(names(samples),1,6)=="I_90_1" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  temp <- apply(s_70, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Achieving...", main="Optimal Yield -- Salcha River", lwd=3, ylim=c(0,1))
  temp <- apply(s_80, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(s_90, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[2], old_goal_lo[2], old_goal_up[2], old_goal_up[2])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[2], old_goal_med[2]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSY", cex=1, pos=4)
  # OVERFISHING PROBABILITY PROFILES
  # FOR THE CHENA
  require(Rcpp)
  cppFunction("
            IntegerMatrix get_of_mat(IntegerMatrix x, int r, int c){
              for (int i=0; i<r; i++){
                for (int j=0; j<(c-1); j++){
                  if (x(i,j)==1 & x(i,j+1)==0){
                    x(i,j+1)=1;
                  }
                }
              }
              return x; 
            }"
  )
  op_70 <- get_of_mat(as.matrix(s_70), nrow(s_70), ncol(s_70))
  op_80 <- get_of_mat(as.matrix(s_80), nrow(s_80), ncol(s_80))
  op_90 <- get_of_mat(as.matrix(s_90), nrow(s_90), ncol(s_90))
  temp <- apply(!op_70, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Reducing to...", main="Overfishing Profile -- Chena River", lwd=3, ylim=c(0,1))
  temp <- apply(!op_80, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(!op_90, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[1], old_goal_lo[1], old_goal_up[1], old_goal_up[1])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[1], old_goal_med[1]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSY", cex=1, pos=4)
  # FOR THE SALCHA 
  op_70 <- get_of_mat(as.matrix(c_70), nrow(c_70), ncol(c_70))
  op_80 <- get_of_mat(as.matrix(c_80), nrow(c_80), ncol(c_80))
  op_90 <- get_of_mat(as.matrix(c_90), nrow(c_90), ncol(c_90))
  temp <- apply(!op_70, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Reducing to...", main="Overfishing Profile -- Salcha River", lwd=3, ylim=c(0,1))
  temp <- apply(!op_80, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(!op_90, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[2], old_goal_lo[2], old_goal_up[2], old_goal_up[2])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[2], old_goal_med[2]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSY", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSY", cex=1, pos=4)
  # OPTIMAL RECRUITMENT PROBABILITY PROFILE  
  # FOR THE CHENA
  c_70 <- samples[,substr(names(samples),1,6)=="I_70_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  c_80 <- samples[,substr(names(samples),1,6)=="I_80_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  c_90 <- samples[,substr(names(samples),1,6)=="I_90_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="1"]
  temp <- apply(c_70, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Achieving...", main="Optimal Recruitment -- Chena River", lwd=3, ylim=c(0,1))
  temp <- apply(c_80, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(c_90, MARGIN=2, sum)
  temp <- temp/nrow(c_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[1], old_goal_lo[1], old_goal_up[1], old_goal_up[1])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[1], old_goal_med[1]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSR", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSR", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSR", cex=1, pos=4)
  # FOR THE SALCHA
  s_70 <- samples[,substr(names(samples),1,6)=="I_70_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  s_80 <- samples[,substr(names(samples),1,6)=="I_80_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  s_90 <- samples[,substr(names(samples),1,6)=="I_90_2" &
                    substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)=="2"]
  temp <- apply(s_70, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  plot(S, temp, ty="l", col=rgb(1,0,0), xlab="Number of Spawners", ylab="Probability of Achieving...", main="Optimal Recruitment -- Salcha River", lwd=3, ylim=c(0,1))
  temp <- apply(s_80, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  lines(S, temp, ty="l", col=rgb(0,1,0), lwd=3)
  temp <- apply(s_90, MARGIN=2, sum)
  temp <- temp/nrow(s_70)
  lines(S, temp, ty="l", col=rgb(0,0,1), lwd=3)
  poly_x <- c(old_goal_lo[2], old_goal_lo[2], old_goal_up[2], old_goal_up[2])
  poly_y <- c(0,1,1,0)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  lines(c(old_goal_med[2], old_goal_med[2]), c(0,1), lty=2, lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2], legend_position[2]), col=rgb(1,0,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.06, legend_position[2]-0.06), col=rgb(0,1,0), lwd=3)
  lines(c(legend_position[1], legend_position[1]+2000), c(legend_position[2]-0.12, legend_position[2]-0.12), col=rgb(0,0,1), lwd=3)
  text(x=legend_position[1]+2000, y=legend_position[2], labels="... 70% MSR", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.06, labels="... 80% MSR", cex=1, pos=4)
  text(x=legend_position[1]+2000, y=legend_position[2]-0.12, labels="... 90% MSR", cex=1, pos=4)
  plot.new()
  par(mai=c(0,0,0,0))
  lines(c(0.34, 0.6), c(1, 1), col="black")
  lines(c(0.34, 0.6), c(0, 0), col="black")
  lines(c(0.34, 0.34), c(1,0), col="black" )
  lines(c(0.6, 0.6), c(1,0), col="black" )
  lines(c(0.35, 0.40), c(0.75,0.75), lty=2, lwd=3  )
  text(0.40, 0.75, "  Current Escapement Goal", pos=4, cex=1.6)
  poly_x <- c(0.35, 0.40, 0.40, 0.35)
  poly_y <- c(0.40, 0.40, 0.1, 0.1)
  polygon(poly_x, poly_y, col=rgb(0.5,  0.5, 0.5, 0.2))
  text(0.40, 0.3, "  Escapement Goal Range", pos=4, cex=1.6)
  dev.off()
}



