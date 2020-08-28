#' Format the data to run a paired analysis
#' @description The output of the function will be used as data within the Jags model. Function is used internally within get_jags_data
#' @export
format.paired <- function(data){
  
  chena_data <- data$chena_data
  salcha_data <- data$salcha_data
  my_data <- data$my_data
  
  n_years <- nrow(my_data)
  
  n_ages <- 8
  
  log_N_hat_mr <- data.frame(Chena=log(chena_data$mr_est_c),
                             Salcha=log(salcha_data$mr_est_s))
  rownames(log_N_hat_mr) <- my_data$year
  
  log_N_hat_tow <- data.frame(Chena=log(chena_data$tow_est_c),
                              Salcha=log(salcha_data$tow_est_s))
  rownames(log_N_hat_tow) <- my_data$year
  
  mr_cv <- data.frame(Chena=chena_data$cv_mr_c,
                      Salcha=salcha_data$cv_mr_s)
  mr_cv[is.na(mr_cv)] <- -999
  rownames(mr_cv) <- my_data$year
  
  tow_cv <- data.frame(Chena=chena_data$cv_tow_c,
                       Salcha=salcha_data$cv_tow_s)
  tow_cv[is.na(tow_cv)] <- -999
  rownames(tow_cv) <- my_data$year
  
  H_hat_1 <- my_data$h_my_est
  names(H_hat_1) <- my_data$year
  
  H_hat_2 <- data.frame(Chena=chena_data$h_est_c,
                        Salcha=salcha_data$h_est_s)
  for(i in 1:nrow(H_hat_2)){
    for (j in 1:ncol(H_hat_2)){
      if (!is.na(H_hat_2[i, j])){
        if (H_hat_2[i, j]==-Inf){
          H_hat_2[i,j] <- -999
        }
      }
    }
  }
  rownames(H_hat_2) <- my_data$year
  
  se_H_hat_1 <- my_data$se_h_my
  names(se_H_hat_1) <- my_data$year
  se_H_hat_1[is.na(se_H_hat_1)] <- -999 # Could throw a little flag
  
  se_H_hat_2 <- data.frame(Chena=chena_data$se_h_c,
                           Salcha=salcha_data$se_h_s)
  se_H_hat_2[is.na(se_H_hat_2)] <- -999
  rownames(se_H_hat_2) <- my_data$year
  
  N_hat_pm <- array(NA, dim = c(n_years,2,6),
                    dimnames = list(my_data$year, 
                                    c("Chena", "Salcha"),
                                    3:8))
  q <- data.frame(my_data$q3,
                  my_data$q4,
                  my_data$q5,
                  my_data$q6,
                  my_data$q7,
                  my_data$q8)
  for(j in 1:2){
    for(k in 1:6){
      N_hat_pm[,j,k] <- q[,k]
    }
  }
  
  N_hat_pm_dot <- data.frame(my_data$n_aged_my, my_data$n_aged_my)
  
  for (i in 1:nrow(N_hat_pm_dot)){
    for (j in 1:ncol(N_hat_pm_dot)){
      for (k in 1:6){
        N_hat_pm[i,j,k] <- round(N_hat_pm[i,j,k]*N_hat_pm_dot[i,j])
      }
    }
  }
  
  N_hat_pm_dot <- apply(N_hat_pm, MARGIN=c(1,2), FUN=sum)
  
  N_hat_pm_dot[is.na(N_hat_pm_dot)] <- 0
  
  rownames(N_hat_pm_dot) <- my_data$year
  
  N_hat_q <- data.frame(Chena = chena_data$n_my_c,
                        Salcha = salcha_data$n_my_s)
  rownames(N_hat_q) <- my_data$year 
  
  N_hat_t <- chena_data$n_my_samples
  N_hat_t[is.na(N_hat_t)] <- 0
  names(N_hat_t) <- my_data$year 
  
  N_hat_pr <- array(NA, dim = c(n_years,2,6),
                    dimnames = list(my_data$year, 
                                    c("Chena", "Salcha"),
                                    3:8))
  p_chena <- data.frame(p3_c = chena_data$p3_c,
                        p4_c = chena_data$p4_c,
                        p5_c = chena_data$p5_c,
                        p6_c = chena_data$p6_c,
                        p7_c = chena_data$p7_c,
                        p8_c = chena_data$p8_c)
  p_salcha <- data.frame(p3_s = salcha_data$p3_s,
                         p4_s = salcha_data$p4_s,
                         p5_s = salcha_data$p5_s,
                         p6_s = salcha_data$p6_s,
                         p7_s = salcha_data$p7_s,
                         p8_s = salcha_data$p8_s)
  for (k in 1:6){
    N_hat_pr[, 1, k] <- p_chena[,k]
    N_hat_pr[, 2, k] <- p_salcha[,k]
  }
  
  N_hat_pr_dot <- data.frame(Chena = chena_data$n_aged_c,
                             Salcha = salcha_data$n_aged_s)
  rownames(N_hat_pr_dot) <- my_data$year 
  
  for (i in 1:nrow(N_hat_pm_dot)){
    for (j in 1:ncol(N_hat_pm_dot)){
      for (k in 1:6){
        N_hat_pr[i,j,k] <- round(N_hat_pr[i,j,k]*N_hat_pr_dot[i,j])
      }
    }
  }
  N_hat_pr_dot <- apply(N_hat_pr, MARGIN=c(1,2), FUN=sum)
  
  N_hat_pr_dot[is.na(N_hat_pr_dot)] <- 0
  
  list(n_years = n_years, 
       n_ages = n_ages, 
       log_N_hat_mr = log_N_hat_mr,
       log_N_hat_tow = log_N_hat_tow,
       mr_cv = mr_cv,
       tow_cv = tow_cv,
       H_hat_1 = H_hat_1,
       H_hat_2 = H_hat_2,
       se_H_hat_1 = se_H_hat_1,
       se_H_hat_2 = se_H_hat_2,
       N_hat_q = N_hat_q,
       N_hat_t = N_hat_t,
       N_hat_pr = N_hat_pr,
       N_hat_pr_dot = N_hat_pr_dot,
       N_hat_pm = N_hat_pm,
       N_hat_pm_dot = N_hat_pm_dot)
  
}

#' Format the data to run a paired analysis
#' @description The output of the function will be used as data within the Jags model. Function is used internally within get_jags_data
#' @export
format.standalone <- function(data){
  
  river_data <- data$river_data
  names(river_data) <- c("year",
                         "mr_est",
                         "cv_mr",
                         "tow_est",
                         "cv_tow",
                         "h_est",
                         "se_h",
                         "n_my_riv",
                         "n_my_samples",
                         "p3",
                         "p4",
                         "p5",
                         "p6",
                         "p7",
                         "p8",
                         "n_aged")
  my_data <- data$my_data
  
  river <- attr(data, "river")
  
  n_years <- nrow(my_data)
  
  n_ages <- 8
  
  log_N_hat_mr <- data.frame(log(river_data$mr_est))
  colnames(log_N_hat_mr) <- river
  rownames(log_N_hat_mr) <- my_data$year
  
  log_N_hat_tow <- data.frame(log(river_data$tow_est))
  colnames(log_N_hat_tow) <- river
  rownames(log_N_hat_tow) <- my_data$year
  
  mr_cv <- data.frame(river_data$cv_mr)
  colnames(mr_cv) <- river
  rownames(mr_cv) <- my_data$year
  mr_cv[is.na(mr_cv)] <- -999
  
  tow_cv <- data.frame(river_data$cv_mr)
  colnames(tow_cv) <- river
  rownames(tow_cv) <- my_data$year
  tow_cv[is.na(tow_cv)] <- -999
  
  H_hat_1 <- my_data$h_my_est
  H_hat_1 <- my_data$year
  
  H_hat_2 <- data.frame(river_data$h_est)
  for(i in 1:nrow(H_hat_2)){
    for (j in 1:ncol(H_hat_2)){
      if (!is.na(H_hat_2[i, j])){
        if (H_hat_2[i, j]==-Inf){
          H_hat_2[i,j] <- -999
        }
      }
    }
  }
  
  se_H_hat_1 <- my_data$se_h_my
  names(se_H_hat_1) <- my_data$year 
  se_H_hat_1[is.na(se_H_hat_1)] <- -999 # Could throw a little flag
  
  se_H_hat_2 <- data.frame(river_data$se_h)
  se_H_hat_2[is.na(se_H_hat_2)] <- -999
  
  rownames(se_H_hat_2) <- my_data$year
  
  colnames(tow_cv) <- river
  rownames(tow_cv) <- my_data$year  
  
  N_hat_q <- data.frame(river_data$n_my_riv)
  colnames(N_hat_q) <- river
  rownames(N_hat_q) <- my_data$year
  
  N_hat_t <- river_data$n_my_samples
  N_hat_t[is.na(N_hat_t)] <- 0
  names(N_hat_t) <- my_data$year
  
  N_hat_pr <- array(NA, dim = c(n_years,1,6),
                    dimnames = list(my_data$year, 
                                    river,
                                    3:8))
  p_riv <- data.frame(river_data$p3,
                      river_data$p4,
                      river_data$p5,
                      river_data$p6,
                      river_data$p7,
                      river_data$p8)
  for(k in 1:6){
    N_hat_pr[,1,k] <- p_riv[,k]
  }
  
  N_hat_pr_dot <- data.frame(river_data$n_aged)
  colnames(N_hat_pr_dot) <- river
  rownames(N_hat_pr_dot) <- my_data$year
  
  for (i in 1:nrow(N_hat_pr_dot)){
    for (j in 1:ncol(N_hat_pr_dot)){
      for (k in 1:6){
        N_hat_pr[i,j,k] <- round(N_hat_pr[i,j,k]*N_hat_pr_dot[i,j])
      }
    }
  }
  N_hat_pr_dot <- apply(N_hat_pr, MARGIN=c(1,2), FUN=sum)
  
  N_hat_pr_dot[is.na(N_hat_pr_dot)] <- 0
  
  N_hat_pm <- array(NA, dim = c(n_years,1,6),
                    dimnames = list(my_data$year, 
                                    river,
                                    3:8))
  q <- data.frame(my_data$q3,
                  my_data$q4,
                  my_data$q5,
                  my_data$q6,
                  my_data$q7,
                  my_data$q8)
  for(k in 1:6){
    N_hat_pm[,1,k] <- q[,k]
  }
  
  N_hat_pm_dot <- data.frame(my_data$n_aged_my)
  colnames(N_hat_pm_dot) <- "Middle Yukon"
  rownames(N_hat_pm_dot) <- my_data$year
  
  for (i in 1:nrow(N_hat_pm_dot)){
    for (j in 1:ncol(N_hat_pm_dot)){
      for (k in 1:6){
        N_hat_pm[i,j,k] <- round(N_hat_pm[i,j,k]*N_hat_pm_dot[i,j])
      }
    }
  }
  N_hat_pm_dot <- apply(N_hat_pm, MARGIN=c(1,2), FUN=sum)
  N_hat_pm_dot[is.na(N_hat_pm_dot)] <- 0
  list(n_years = n_years, 
       n_ages = n_ages, 
       log_N_hat_mr = log_N_hat_mr,
       log_N_hat_tow = log_N_hat_tow,
       mr_cv = mr_cv,
       tow_cv = tow_cv,
       H_hat_1 = H_hat_1,
       H_hat_2 = H_hat_2,
       se_H_hat_1 = se_H_hat_1,
       se_H_hat_2 = se_H_hat_2,
       N_hat_q = N_hat_q,
       N_hat_t = N_hat_t,
       N_hat_pr = N_hat_pr,
       N_hat_pr_dot = N_hat_pr_dot,
       N_hat_pm = N_hat_pm,
       N_hat_pm_dot = N_hat_pm_dot)
  
}

