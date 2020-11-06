#' Extract abundance estimates from Jags output
#' @param samples (data.frame) output of clean_chains
#' @param alpha (numeric) desired significance level for credible intervals
#' @param file_path (character) file path to store output xlsx file.
#' @export
extract_abundance_estimates <- function(samples, alpha=0.10, file_path){
  final_year=2030
  chena_total <- samples[,substr(names(samples), 1, 7) == "N_1_dot" & 
                          substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==1]
  salcha_total <- samples[,substr(names(samples), 1, 7) == "N_1_dot" & 
                           substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==2]
  
  chena_inriver <- samples[,substr(names(samples), 1, 3) == "N_2" & 
                            substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==1]
  salcha_inriver <- samples[,substr(names(samples), 1, 3) == "N_2" & 
                             substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==2]
  
  chena_escapement <- samples[,substr(names(samples), 1, 2) == "S[" & 
                               substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==1 &
                               !is.element(substr(names(samples), 3, 3), c("e", "m"))]
  salcha_escapement <- samples[,substr(names(samples), 1, 2) == "S[" & 
                                substr(names(samples), nchar(names(samples))-1, nchar(names(samples))-1)==2 &
                                !is.element(substr(names(samples), 3, 3), c("e", "m"))]
  
  ct_quants <- round(apply(chena_total, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  st_quants <- round(apply(salcha_total, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  ci_quants <- round(apply(chena_inriver, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  si_quants <- round(apply(salcha_inriver, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  ce_quants <- round(apply(chena_escapement, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  se_quants <- round(apply(salcha_escapement, MARGIN=2 , quantile, probs = c(0.5, alpha/2, 1-alpha/2)), 0)
  
  chena_abundance <- data.frame(year=1986:final_year, total_run = rep(NA, length(1986:final_year)), inriver_run = rep(NA, length(1986:final_year)), escapement = rep(NA, length(1986:final_year)))
  salcha_abundance <-  data.frame(year=1986:final_year, total_run = rep(NA, length(1986:final_year)), inriver_run = rep(NA, length(1986:final_year)), escapement = rep(NA, length(1986:final_year)))
  
  up <- which((1986:2030)==final_year)
  
  chena_abundance[9:up, 2] <- paste(ct_quants[1,], " (", ct_quants[2,], ", " ,  ct_quants[3,], ")", sep="")[1:(up-8)]
  salcha_abundance[9:up, 2] <- paste(st_quants[1,], " (", st_quants[2,], ", " ,  st_quants[3,], ")", sep="")[1:(up-8)]
  chena_abundance[1:up,3] <- paste(ci_quants[1,], " (", ci_quants[2,], ", " ,  ci_quants[3,], ")", sep="")[1:up]
  salcha_abundance[1:up,3] <- paste(si_quants[1,], " (", si_quants[2,], ", " ,  si_quants[3,], ")", sep="")[1:up]
  chena_abundance[1:up,4] <- paste(ce_quants[1,], " (", ce_quants[2,], ", " ,  ce_quants[3,], ")", sep="")[1:up]
  salcha_abundance[1:up,4] <- paste(se_quants[1,], " (", se_quants[2,], ", " ,  se_quants[3,], ")", sep="")[1:up]
  
  writexl::write_xlsx(list(chena=chena_abundance, salcha=salcha_abundance), path=file_path)
}
