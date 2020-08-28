get_inits <- function(){
  log_alpha <- c(2.075, 2.303)
  alpha <- exp(log_alpha)
  beta <- c(0.00020, 0.00018)
  list(alpha=alpha, beta=beta)
}



