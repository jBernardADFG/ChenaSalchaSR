#' Get a set of initial values for alpha and beta (the initial values are the estimates reported in the last Chena/Salcha report)
#' @param model (character) The model being run
#' @export
get_initial_values <- function(model){
  if (!is.element(model, c("base_tvp", "base_tvm_tvp", "base_tvm_ar_tvp", "base_tvp_ld"))){
    f <- function(){
      log_alpha <- c(2.075, 2.303)
      alpha <- exp(log_alpha)
      beta <- c(0.00020, 0.00018)
      inits <- list(alpha=alpha, beta=beta)
      return(inits)
    }
  }
  else{
    f <- function(){
      log_alpha <- c(2.075, 2.303)
      alpha <- exp(log_alpha)
      a_0 <- alpha
      a_1 <- c(0,0)
      beta <- c(0.00020, 0.00018)
      inits <- list(a_0=a_0, a_1=a_1, beta=beta)
      return(inits)
    }
  }
  return(f)
}



