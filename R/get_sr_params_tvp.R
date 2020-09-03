

# This might be better in a table ... 

tvp_table <- function(samples, alpha=0.10){

  alpha=0.10
  path <- "Tables/sr_params/spooky.xlsx"
  
  log_alpha <- samples[,substr(names(samples), 1, 9)=="log_alpha"]
  log_alpha_c <- log_alpha[,1:(ncol(log_alpha)/2)] 
  log_alpha_s <- log_alpha[,(ncol(log_alpha)/2+1):ncol(log_alpha)]
  
  S_eq <- samples[,substr(names(samples), 1, 4)=="S_eq"]
  S_eq_c <- S_eq[,1:(ncol(log_alpha)/2)] 
  S_eq_s <- S_eq[,(ncol(log_alpha)/2+1):ncol(log_alpha)]
  
  S_msy <- samples[,substr(names(samples), 1, 5)=="S_msy"]
  S_msy_c <- S_msy[,1:(ncol(log_alpha)/2)] 
  S_msy_s <- S_msy[,(ncol(log_alpha)/2+1):ncol(log_alpha)]
  
  R_msy <- samples[,substr(names(samples), 1, 5)=="R_msy"]
  R_msy_c <- R_msy[,1:(ncol(log_alpha)/2)] 
  R_msy_s <- R_msy[,(ncol(log_alpha)/2+1):ncol(log_alpha)]
  
  U_msy <- samples[,substr(names(samples), 1, 5)=="U_msy"]
  U_msy_c <- U_msy[,1:(ncol(log_alpha)/2)] 
  U_msy_s <- U_msy[,(ncol(log_alpha)/2+1):ncol(log_alpha)]
  
  log_alpha_c_med <- round(apply(log_alpha_c, MARGIN=2, median), 2) 
  log_alpha_s_med <- round(apply(log_alpha_s, MARGIN=2, median), 2)
  S_eq_c_med <- round(apply(S_eq_c, MARGIN=2, median))
  S_eq_s_med <- round(apply(S_eq_s, MARGIN=2, median))
  S_msy_c_med <- round(apply(S_msy_c, MARGIN=2, median))
  S_msy_s_med <- round(apply(S_msy_s, MARGIN=2, median))
  R_msy_c_med <- round(apply(R_msy_c, MARGIN=2, median))
  R_msy_s_med <- round(apply(R_msy_s, MARGIN=2, median))
  U_msy_c_med <- round(apply(U_msy_c, MARGIN=2, median), 3)
  U_msy_s_med <- round(apply(U_msy_s, MARGIN=2, median), 3)
  
  log_alpha_c_lo <- round(apply(log_alpha_c, MARGIN=2, quantile, probs=alpha/2), 2) 
  log_alpha_s_lo <- round(apply(log_alpha_s, MARGIN=2, quantile, probs=alpha/2), 2)
  S_eq_c_lo <- round(apply(S_eq_c, MARGIN=2, quantile, probs=alpha/2))
  S_eq_s_lo <- round(apply(S_eq_s, MARGIN=2, quantile, probs=alpha/2))
  S_msy_c_lo <- round(apply(S_msy_c, MARGIN=2, quantile, probs=alpha/2))
  S_msy_s_lo <- round(apply(S_msy_s, MARGIN=2, quantile, probs=alpha/2))
  R_msy_c_lo <- round(apply(R_msy_c, MARGIN=2, quantile, probs=alpha/2))
  R_msy_s_lo <- round(apply(R_msy_s, MARGIN=2, quantile, probs=alpha/2))
  U_msy_c_lo <- round(apply(U_msy_c, MARGIN=2, quantile, probs=alpha/2), 3)
  U_msy_s_lo <- round(apply(U_msy_s, MARGIN=2, quantile, probs=alpha/2), 3)
  
  log_alpha_c_up <- round(apply(log_alpha_c, MARGIN=2, quantile, probs=1-alpha/2), 2) 
  log_alpha_s_up <- round(apply(log_alpha_s, MARGIN=2, quantile, probs=1-alpha/2), 2)
  S_eq_c_up <- round(apply(S_eq_c, MARGIN=2, quantile, probs=1-alpha/2))
  S_eq_s_up <- round(apply(S_eq_s, MARGIN=2, quantile, probs=1-alpha/2))
  S_msy_c_up <- round(apply(S_msy_c, MARGIN=2, quantile, probs=1-alpha/2))
  S_msy_s_up <- round(apply(S_msy_s, MARGIN=2, quantile, probs=1-alpha/2))
  R_msy_c_up <- round(apply(R_msy_c, MARGIN=2, quantile, probs=1-alpha/2))
  R_msy_s_up <- round(apply(R_msy_s, MARGIN=2, quantile, probs=1-alpha/2))
  U_msy_c_up <- round(apply(U_msy_c, MARGIN=2, quantile, probs=1-alpha/2), 3)
  U_msy_s_up <- round(apply(U_msy_s, MARGIN=2, quantile, probs=1-alpha/2), 3)
  
  log_alpha_c <- paste(log_alpha_c_med, " (", log_alpha_c_lo, ", ", log_alpha_c_up, ")", sep="")
  log_alpha_s <- paste(log_alpha_s_med, " (", log_alpha_s_lo, ", ", log_alpha_s_up, ")", sep="")
  S_eq_c <- paste(S_eq_c_med, " (", S_eq_c_lo, ", ", S_eq_c_up, ")", sep="")
  S_eq_s <- paste(S_eq_s_med, " (", S_eq_s_lo, ", ", S_eq_s_up, ")", sep="")
  S_msy_c <- paste(S_msy_c_med, " (", S_msy_c_lo, ", ", S_msy_c_up, ")", sep="")
  S_msy_s <- paste(S_msy_s_med, " (", S_msy_s_lo, ", ", S_msy_s_up, ")", sep="")
  R_msy_c <- paste(R_msy_c_med, " (", R_msy_c_lo, ", ", R_msy_c_up, ")", sep="")
  R_msy_s <- paste(R_msy_s_med, " (", R_msy_s_lo, ", ", R_msy_s_up, ")", sep="")
  U_msy_c <- paste(U_msy_c_med, " (", U_msy_c_lo, ", ", U_msy_c_up, ")", sep="")
  U_msy_s <- paste(U_msy_s_med, " (", U_msy_s_lo, ", ", U_msy_s_up, ")", sep="")
  
  df_c <- data.frame("year" = 1986:(1985+length(log_alpha_c)),
                     "log_alpha" = log_alpha_c, 
                     "S_eq" = S_eq_c,
                     "S_msy" = S_msy_c,
                     "R_msy" = R_msy_c,
                     "U_msy" = U_msy_c)
  df_s <- data.frame("year" = 1986:(1985+length(log_alpha_s)), 
                     "log_alpha" = log_alpha_s, 
                     "S_eq" = S_eq_s,
                     "S_msy" = S_msy_s,
                     "R_msy" = R_msy_s,
                     "U_msy" = U_msy_s)
  df <- list(chena=df_c, salcha=df_s)
  writexl::write_xlsx(df, path=path)
  
}

