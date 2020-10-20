#' Base Ricker with time varying productivity
#' @param path (character) path to write .jags file
#' @export
write_jags_model.base_tvm_tvp <- function(path){
  mod <-
    "model{

    ########################################################################
    ############################ LATENT PROCESS ############################
    ########################################################################

    # ------------------------------------------
    # IN-RIVER-RUN-ABUNDANCE ON THE CHENA AND SALCHA DURING THE INITIAL YEARS #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_ages){
        N_2[y,r] ~ dnorm(mu_N[r], tau_N[r])T(0,)
      }
      mu_N[r] ~ dunif(1000,15000)
      tau_N[r] <- pow(1/sig_N[r], 2)
      sig_N[r] ~ dexp(1E-4)
    }
  
    # ------------------------------------------
    # HARVEST ON THE CHENA AND SALCHA #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){                                                    
        H_2[y,r] ~ dnorm(mu_2[r], tau_2[r])T(0,N_2[y,r])
      }
      mu_2[r] ~ dunif(0, 2000)
      tau_2[r] <- pow(1/sig_2[r], 2)
      sig_2[r] ~ dexp(1E-4)
    }
  
    # ------------------------------------------
    # SPAWNERS GIVEN IN-RIVER-RUN ABUNDANCE AND HARVEST ON THE CHENA AND SALCHA #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){                               
        S[y,r] <- max(N_2[y,r]-H_2[y,r], 0.0001)
      }
    }
  
    # ------------------------------------------
    # RS PROCESSES
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){
        log_R[y,r] ~ dnorm(mu_sr[y,r], tau_w[r])
        mu_sr[y,r] <- log(a_0[r]+a_1[r]*(y-1)) + log(S[y,r]) - beta[r]*S[y,r]
        alpha[y,r] <- a_0[r]+a_1[r]*(y-1)
        log_alpha[y,r] <- log(alpha[y,r])
        R[y,r] <- exp(log_R[y,r])
        nu[y,r] <- log_R[y,r]-log(alpha[y,r])-log(S[y,r])+beta[r]*S[y,r]
      }
      tau_w[r] <- pow(1/sig_w[r], 2)
      sig_w[r] ~ dexp(0.1)
      a_0[r] ~ dunif(1, 20)
      a_1[r] ~ dunif(-a_0[r]/46, 10)
      # a_1[r] ~ dunif((1-a_0[r])/(n_years-1), 10)
      beta[r] ~ dexp(1E2)
    }

    # ------------------------------------------
    # RETURNERS GIVEN RECRUITS #
    # ------------------------------------------
    for (r in 1:2){
      for (y in (n_ages+1):n_years){
        for (a in 1:6){
          N_1[y,r,a] <- R[(y-(a+2)),r]*p[(y-(a+2)),r,a]
        }
        N_1_dot[y, r] <- sum(N_1[y,r,1:6])
      }
    }

    # ------------------------------------------
    # Age-at-maturity probability vector  
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){
        p[y,r,1:6] ~ ddirch(gamma[y,r,1:6]+0.1)
        for (a in 1:6){
          gamma[y,r,a] <- pi[y,r,a]*D[r]
          pi[y,r,a] <- logistic[y,r,a]/sum(logistic[y,r,1:6])
          logistic[y,r,a] <- exp(n[1,r,a]+n[2,r,a]*y)
        }
      }
      D[r] ~ dexp(0.001)T(1,)
      for (a in 1:6){
        n[1,r,a] ~ dunif(-1000, 1000)
        n[2,r,a] ~ dunif((-1000-n[1,r,a])/n_years, (1000-n[1,r,a])/n_years)
      }
    }

    # --------------
    # IRRA GIVEN RETURNERS AND MIDDLE YUKON HARVEST #
    # --------------
    for (r in 1:2){
      for (y in (n_ages+1):n_years){
        N_2[y,r] <- max(N_1_dot[y,r]-q[y,r]*H_1[y], 0.0001) 
      }
    }
    for (r in 1:2){
      for (y in 1:n_ages){
        q[y,r] ~ dnorm(mu_q[r], tau_q[r])
      }
      for (y in (n_ages+1):n_years){
        q[y,r] ~ dnorm(mu_q[r], tau_q[r])T(0,min(1,N_1_dot[y,r]/H_1[y]))
      }
      mu_q[r] ~ dunif(0,1)
      tau_q[r] <- pow(1/sig_q[r],2)
      sig_q[r] ~ dexp(0.001)
    }
    
    # --------------
    # MIDDLE YUKON HARVEST #
    # --------------
    for (y in 1:n_years){
      H_1[y] ~ dnorm(mu_1, tau_1)T(0,)
    }
    mu_1 ~ dunif(0, 30000)
    tau_1 <- pow(1/sig_1, 2)
    sig_1 ~ dexp(1E-5)  
  
    #############################################################################
    ############################ OBSERVATION PROCESS ############################
    #############################################################################
    
    for (y in 1:n_years){
      # ------------------------------------------
      # MARK-RECAPTURE ABUNDANCE ESTIMATES #
      # ------------------------------------------
      log_N_hat_mr[y, 1] ~ dnorm(log(N_2[y,1]) - delta[y], tau_mr[y,1])
      log_N_hat_mr[y, 2] ~ dnorm(log(N_2[y,2]), tau_mr[y,2])
      delta[y] ~ dexp(lambda)
      for(r in 1:2){
        tau_mr[y,r] <- 1/var_mr[y,r]
        var_mr[y,r] <- log(pow(mr_cv[y,r], 2)+1)
      }
    }
    lambda ~ dexp(0.01)
    
    for(r in 1:2){
      for (y in 1:n_years){
    
        # ------------------------------------------
        # TOWER COUNTS #
        # ------------------------------------------
        log_N_hat_tow[y, r] ~ dnorm(log(N_2[y,r]), tau_tow[y,r])
        tau_tow[y,r] <- 1/var_tow[y,r]
        var_tow[y,r] <- log(pow(tow_cv[y,r],2)+1)
    
        # ------------------------------------------
        # CHENA AND SALCHA HARVEST #
        # ------------------------------------------
        H_hat_2[y,r] ~ dnorm(H_2[y,r], tau_2_star[y,r])T(0,)
        tau_2_star[y,r] <- pow(1/sig_2_star[y,r], 2)
        sig_2_star[y,r] <- se_H_hat_2[y,r]
    
        # ------------------------------------------
        # MOVEMENT BETWEEN THE MIDDLE YUKON AND THE CHENA AND SALCHA #
        # ------------------------------------------
        N_hat_q[y,r] ~ dbin(q[y,r], N_hat_t[y])
      
      }
      
      # ------------------------------------------
      # AGE DATA FROM THE CHENA AND SALCHA #
      # ------------------------------------------
      for (y in (n_ages+1):n_years){
        N_hat_pr[y, r, 1:6] ~ dmulti(
          c(p[y-3,r,1], p[y-4,r,2], p[y-5,r,3], p[y-6,r,4], p[y-7,r,5], p[y-8,r,6]),
          N_hat_pr_dot[y,r]
        )
      }
      
    }
  
    # ------------------------------------------
    # HARVEST IN THE MIDDLE YUKON #
    # ------------------------------------------
    for (y in 1:n_years){
      H_hat_1[y] ~ dnorm(H_1[y], tau_1_star[y])T(0,)
      tau_1_star[y] <- pow(1/sig_1_star[y], 2)
      sig_1_star[y] ~ dunif(0, se_H_hat_1[y])
    }
  
    ############################################################################################
    ############################ CALCULATING SOME USEFUL STATISTICS ############################
    ############################################################################################

    for (r in 1:2){
  
      # --------------
      # TIME VARYING PRODUCTIVITY WITHOUT THE AR(1) TERM #
      for (y in 1:n_years){
       alpha_prime[y,r] <- alpha[y,r]*exp(pow(sig_w[r], 2)/2)
      }
    
      # --------------
      # RICKER RS RELATIONSHIP WITH TIME VARYING PRODUCTIVITY PARAMETER #
      for (y in 1:n_years){
        S_msy[y,r] <- log(alpha_prime[y,r])/beta[r]*(0.5-0.07*log(alpha_prime[y,r]))
        R_msy[y,r] <- alpha_prime[y,r]*S_msy[y,r]*exp(-beta[r]*S_msy[y,r])
        MSY[y,r] <- R_msy[y, r]-S_msy[y, r]
        S_max[y,r] <- 1/beta[r]
        S_eq[y,r] <- log(alpha_prime[y,r])/beta[r]
        MSR[y, r] <- alpha_prime[y, r]*S_max[y, r]*exp(-beta[r]*S_max[y, r])
        U_msy[y,r] <- log(alpha_prime[y,r])*(0.5-0.07*log(alpha_prime[y,r]))
      }

    }

    ###################################################################################################################################  
    ############################  OPTIMAL YIELD, OVERFISHING, AND OPTIMAL RECRUITMENT PROBABILITY PROFILES ############################ 
    ###################################################################################################################################  
    for (i in 1:450){
      S_star[i] <- 50*i
      for (r in 1:2){
        for (y in 1:n_years){
          R_star[i,y,r] <- alpha_prime[y, r]*S_star[i]*exp(-beta[r]*S_star[i])
          SY[i,y,r] <- R_star[i,y,r]-S_star[i]
      
          # FOR OPTIMAL YIELD AND OVERFISHING PROFILES #
          I_90_1[i,y,r] <- step(SY[i,y,r]-0.9*MSY[y,r])
          I_80_1[i,y,r] <- step(SY[i,y,r]-0.8*MSY[y,r])
          I_70_1[i,y,r] <- step(SY[i,y,r]-0.7*MSY[y,r])
    
          # FOR OPTIMAL RECRUITMENT PROFILE #
          I_90_2[i,y,r] <- step(R_star[i,y,r]-0.9*MSR[y,r])
          I_80_2[i,y,r] <- step(R_star[i,y,r]-0.8*MSR[y,r])
          I_70_2[i,y,r] <- step(R_star[i,y,r]-0.7*MSR[y,r])
        }
      }
    }
  }"
  
  writeLines(mod,con=path)
}
