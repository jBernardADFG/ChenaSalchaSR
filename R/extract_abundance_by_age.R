#' Extract estimates of total run abundance by age from Jags output
#' @param samples (data.frame) output of clean_chains.
#' @param alpha (numeric) desired significance level for credible intervals.
#' @param file_path (character) file path to store output xlsx file.
#' @export
extract_abundance_by_age <- function(samples, alpha, file_path){
  aba_samples <- samples[,substr(names(samples), 1, 3)=="N_1" &
                          substr(names(samples), 4, 4)!="_"]
  quants <- apply(aba_samples, MARGIN=2, quantile, probs = c(0.5, alpha/2, 1-alpha/2) )
  med_array <- lb_array <- ub_array <- array(NA, dim=c(27, 2, 6))
  l <- 1
  for(k in 1:6){
    for(j in 1:2){
      for(i in 1:27){
        med_array[i, j, k] <- round(quants[1,l], 0) 
        lb_array[i, j, k] <- round(quants[2,l], 0)
        ub_array[i, j, k] <- round(quants[3,l], 0)
        l <- l + 1
      }
    }
  }
  chena_tab <- salcha_tab <- data.frame(
    year=1994:2020,
    age_3=rep(NA, 27),
    age_4=rep(NA, 27),
    age_5=rep(NA, 27),
    age_6=rep(NA, 27),
    age_7=rep(NA, 27),
    age_8=rep(NA, 27)
  )
  for (i in 1:27){
    for (k in 1:6){
      chena_tab[i, k+1] <- paste(med_array[i, 1, k], " (", lb_array[i, 1, k], ", ", ub_array[i, 1, k], ")", sep="")
      salcha_tab[i, k+1] <- paste(med_array[i, 2, k], " (", lb_array[i, 2, k], ", ", ub_array[i, 2, k], ")", sep="")
    }
  }
  writexl::write_xlsx(list(chena=chena_tab, salcha=salcha_tab), path=file_path)
}

