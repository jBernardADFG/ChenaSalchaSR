#' Extract estimates of total run abundance by age from Jags output
#' @param samples (data.frame) output of clean_chains
#' @param alpha (numeric) desired significance level for credible intervals
#' @param file_path (character) file path to store output xlsx file.
extract_age_at_maturity <- function(samples, alpha, file_path){
  aam_samples <- samples[,substr(names(samples), 1, 1) == "p" &
                          substr(names(samples), 2, 2) != "h"]
  quants <- apply(aam_samples, MARGIN=2, quantile, probs = c(0.5, alpha/2, 1-alpha/2) )
  med_array <- lb_array <- ub_array <- array(NA, dim=c(35, 2, 6))
  
  l <- 1
  for(k in 1:6){
    for(j in 1:2){
      for(i in 1:35){
        med_array[i, j, k] <- round(quants[1,l], 2) 
        lb_array[i, j, k] <- round(quants[2,l], 2)
        ub_array[i, j, k] <- round(quants[3,l], 2)
        l <- l + 1
      }
    }
  }
  
  chena_tab <- salcha_tab <- data.frame(
    year=1986:2020,
    age_3=rep(NA, 35),
    age_4=rep(NA, 35),
    age_5=rep(NA, 35),
    age_6=rep(NA, 35),
    age_7=rep(NA, 35),
    age_8=rep(NA, 35)
  )
  
  for (i in 1:35){
    for (k in 1:6){
      chena_tab[i, k+1] <- paste(med_array[i, 1, k], " (", lb_array[i, 1, k], ", ", ub_array[i, 1, k], ")", sep="")
      salcha_tab[i, k+1] <- paste(med_array[i, 2, k], " (", lb_array[i, 2, k], ", ", ub_array[i, 2, k], ")", sep="")
    }
  }
  
  writexl::write_xlsx(list(chena=chena_tab, salcha=salcha_tab), path=file_path)
  
}
