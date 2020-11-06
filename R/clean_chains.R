#' Convert chains to an easier to use format and burnin further if the burnin period was not long enough
#' @param samples Output of jagsUI::jags
#' @param burnin_further (numeric) The number of additional MCMC iterations to discard as burnin
#' @export
clean_chains <- function(samples, burnin_further=0){
  samps <- samples$samples
  for (i in 1:length(samps)){
    samps[[i]] <- samps[[i]][(burnin_further+1):nrow(samps[[i]]),]
  }
  s <- data.frame()
  for (i in 1:length(samps)){
    s <- rbind(s, samps[[i]])
  }
  return(s)
}
