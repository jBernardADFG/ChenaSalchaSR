#' Extracts and formats end values of chains so that they can be used as initial values in new run
#' @param jags_out Output of jagsUI::jags
#' @param n_chains Number of chains in MCMC routine
#' @export
get_new_inits <- function(jags_out, n_chains){
  new_inits <- list()
  for (i in 1:n_chains){
    for (j in 1:length(jags_out$mcmc.info$end.values)){
      new_inits[[i]] <- as.list(jags_out$mcmc.info$end.values[[i]])
    }
  }
  return(new_inits)
}

