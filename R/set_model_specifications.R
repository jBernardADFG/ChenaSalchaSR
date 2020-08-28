#' Specify important information related to model run
#' @param run_name (character) run identifier for output tables and plots. keep it short and informative
#' @param notes (character) store a note related to the model run to keep track of why the model is being run with the chosen specs 
#' @param n_chains (numeric) the number of chains
#' @param n_iter (numeric) the number of iterations including burnin
#' @param n_burnin (numeric) the number of iterations to discard as burnin
#' @param n_thin (numeric) the thinning period
#' @param parallel (logical) use parallel = F, or find a working wrapper for parallel computation. jagsUI::jags is a little buggy when parallel=T
#' @export
set_model_specifications <- function(model_name, notes, n_chains = 4, n_iter = 500000, n_burnin = 250000, n_thin = 100, parallel = F){
  list(model_name = model_name,
       notes = notes,
       n_chains = n_chains, 
       n_iter = n_iter, 
       n_burnin = n_burnin,
       n_thin = n_thin,
       parallel = parallel)
}