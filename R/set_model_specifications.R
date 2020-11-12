#' Specify how the model should be run and document your thinking in log book (WorkingDirectory/Tables/run_info).
#' @param run_name (character) run identifier. keep it short and informative.
#' @param notes (character) note to be stored in log book 
#' @param n_chains (numeric) the number of chains
#' @param n_iter (numeric) the number of iterations excluding burnin
#' @param n_burnin (numeric) the number of iterations to discard as burnin
#' @param n_thin (numeric) the thinning period
#' @param parallel (logical) use parallel = T for parallel chains
#' @export
set_model_specifications <- function(run_name, notes, n_chains = 4, n_iter = 500000, n_burnin = 250000, n_thin = 100, parallel = F){
  list(run_name = run_name,
       notes = notes,
       n_chains = n_chains, 
       n_iter = n_iter, 
       n_burnin = n_burnin,
       n_thin = n_thin,
       parallel = parallel)
}
