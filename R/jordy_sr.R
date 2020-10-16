jordy_sr <- function(S, S_star, alpha){
  R <- rep(NA, length(S))
  for (i in 1:length(S)){
    if (S[i] < S_star){
      R[i] <- alpha/(2*S_star)*S[i]^2
    }else{
      R[i] <- alpha*S_star*(log(S[i])+1/2-log(S_star))
    }
  }
  return(R)
}
S <- 1:10000
S_star <- 1500

alpha <- 2.25
R <- jordy_sr(S, S_star, alpha)
plot(S, R, ty="l", col="red")
lines(S, S)
S_msy <- alpha*S_star
R_msy <- jordy_sr(S_msy, S_star, alpha)
points(S_msy, R_msy, pch=19, cex=2, col="red")
S_fucked <- 2*S_star/alpha
R_fucked <- jordy_sr(S_fucked, S_star, alpha)
points(S_fucked, R_fucked, pch=19, cex=2, col="red")
S_msy-S_fucked

alpha <- 2
R <- jordy_sr(S, S_star, alpha)
lines(S, R, col="green")
S_msy <- alpha*S_star
R_msy <- jordy_sr(S_msy, S_star, alpha)
points(S_msy, R_msy, pch=19, cex=2, col="green")
S_fucked <- 2*S_star/alpha
R_fucked <- jordy_sr(S_fucked, S_star, alpha)
points(S_fucked, R_fucked, pch=19, cex=2, col="green")
S_msy-S_fucked

alpha <- 1.75
R <- jordy_sr(S, S_star, alpha)
lines(S, R, col="blue")
S_msy <- alpha*S_star
R_msy <- jordy_sr(S_msy, S_star, alpha)
points(S_msy, R_msy, pch=19, cex=2, col="blue")
S_fucked <- 2*S_star/alpha
R_fucked <- jordy_sr(S_fucked, S_star, alpha)
points(S_fucked, R_fucked, pch=19, cex=2, col="blue")
S_msy-S_fucked

alpha <- 1.5
R <- jordy_sr(S, S_star, alpha)
lines(S, R, col="yellow")
S_msy <- alpha*S_star
R_msy <- jordy_sr(S_msy, S_star, alpha)
points(S_msy, R_msy, pch=19, cex=2, col="yellow")
S_fucked <- 2*S_star/alpha
R_fucked <- jordy_sr(S_fucked, S_star, alpha)
points(S_fucked, R_fucked, pch=19, cex=2, col="yellow")
S_msy-S_fucked



jordy_sr <- function(S, S_star, alpha, beta){
  R <- rep(NA, length(S))
  for (i in 1:length(S)){
    if (S[i] < S_star){
      R[i] <- alpha/(2*S_star)*S[i]^2
    }else{
      R[i] <- alpha*(S[i]-S_star)*exp(-beta*(S[i]-S_star))+alpha/2*S_star
    }
  }
  return(R)
}

par(mfrow=c(1,1))
S <- 1:10000
S_star <- 1500
beta <- 0.0002
alpha <- 2.25
R <- jordy_sr(S, S_star, alpha, beta)
plot(S, R, ty="l", col="red")
lines(S, S)

S <- 1:10000
S_star <- 1500
beta <- 0.0002
alpha <- 2.0
R <- jordy_sr(S, S_star, alpha, beta)
lines(S, R, col="blue")

S <- 1:10000
S_star <- 1500
beta <- 0.0002
alpha <- 1.75
R <- jordy_sr(S, S_star, alpha, beta)
lines(S, R, col="green")






