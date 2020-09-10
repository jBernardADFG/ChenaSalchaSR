#' Extract biological reference points and parameters contained in the SR relationship
#' @param samples (data.frame) output of clean_chains
#' @param alpha (numeric) desired significance level for credible intervals
#' @param file_path (character) file path to store output xlsx file
#' @export
extract_sr_params <- function(samples, alpha, file_path, model){
  
  if (is.element(model, c("base", "base_tvm", "base_ar", "base_tvm_ar"))){
    log_alpha_1 <- samples[,names(samples) == "log_alpha[1]"]
    log_alpha_2 <- samples[,names(samples) == "log_alpha[2]"]
    
    beta_1 <- samples[,names(samples) == "beta[1]"]
    beta_2 <- samples[,names(samples) == "beta[2]"]
    
    if (is.element(model, c("base", "base_tvm", "base_tvp", "bev_holt"))){
      phi_1 <- rep(NA, length(beta_1))
      phi_2 <- rep(NA, length(beta_1))
    }else{
      phi_1 <- samples[,names(samples) == "phi[1]"]
      phi_2 <- samples[,names(samples) == "phi[2]"]
    }
    
    sig_w_1 <- samples[,names(samples) == "sig_w[1]"]
    sig_w_2 <- samples[,names(samples) == "sig_w[2]"]
    
    S_max_1 <- samples[,names(samples) == "S_max[1]"]
    S_max_2 <- samples[,names(samples) == "S_max[2]"]
    
    S_eq_1 <- samples[,names(samples) == "S_eq[1]"]
    S_eq_2 <- samples[,names(samples) == "S_eq[2]"]
    
    S_msy_1 <- samples[,names(samples) == "S_msy[1]"] 
    S_msy_2 <- samples[,names(samples) == "S_msy[2]"]
    
    R_msy_1 <- samples[,names(samples) == "R_msy[1]"]
    R_msy_2 <- samples[,names(samples) == "R_msy[2]"]
    
    U_msy_1 <- samples[,names(samples) == "U_msy[1]"]
    U_msy_2 <- samples[,names(samples) == "U_msy[2]"]
    
    chena_params <- data.frame(log_alpha=log_alpha_1, 
                               beta=beta_1,
                               phi=phi_1,
                               sig_w=sig_w_1,
                               S_max=S_max_1,
                               S_eq=S_eq_1,
                               S_msy=S_msy_1,
                               R_msy=R_msy_1,
                               U_msy=U_msy_1)
    
    salcha_params <- data.frame(log_alpha=log_alpha_2, 
                                beta=beta_2,
                                phi=phi_2,
                                sig_w=sig_w_2,
                                S_max=S_max_2,
                                S_eq=S_eq_2,
                                S_msy=S_msy_2,
                                R_msy=R_msy_2,
                                U_msy=U_msy_2)
    
    chena_quants <- apply(chena_params, MARGIN=2, quantile, probs = c(0.5, alpha/2, 1-alpha/2), na.rm=T)
    salcha_quants <- apply(salcha_params, MARGIN=2, quantile, probs = c(0.5, alpha/2, 1-alpha/2), na.rm=T)
    
    chena_quants[,5:8] <- round(chena_quants[,5:8])
    chena_quants[,c(1:4,9)] <- signif(chena_quants[,c(1:4,9)],3)
    
    salcha_quants[,5:8] <- round(salcha_quants[,5:8])
    salcha_quants[,c(1:4,9)] <- signif(salcha_quants[,c(1:4,9)],3)
    
    sr_params <- data.frame(parameter = c("log_alpha", "beta", "phi", "sig_w", "S_max", "S_eq" , "S_msy", "R_msy", "U_msy"),
                            chena = paste(chena_quants[1,], " (", chena_quants[2,], ", ", chena_quants[3,], ")"),
                            salcha = paste(salcha_quants[1,], " (", salcha_quants[2,], ", ", salcha_quants[3,], ")"))
  }
  if (is.element(model, c("base_tvp"))){
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
    sr_params <- list(chena=df_c, salcha=df_s)
  }
  
  writexl::write_xlsx(sr_params, path=file_path)
  
}
