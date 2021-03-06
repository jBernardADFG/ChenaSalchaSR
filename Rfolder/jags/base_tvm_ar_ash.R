#' time varying age-at-maturity, AR(1) term, and age-stratified harvest terms added to base model
#' @param path (character) path to write .jags file
#' @export
write_jags_model.base_tvm_ash <- function(path){
  mod <-
    "model{

    ########################################################################
    ############################ LATENT PROCESS ############################
    ########################################################################

    # ------------------------------------------
    # IN-RIVER-RUN-ABUNDANCE ON THE CHENA AND SALCHA DURING THE INITIAL YEARS
    # ------------------------------------------
    for (r in 1:2){
      for (a in 1:6){
        for (y in 1:n_ages){
          N_2[y,r,a] ~ dnorm(mu_N[r,a], tau_N[r,a])T(0,)
        }
        mu_N[r,a] ~ dunif(0,15000)
        tau_N[r,a] <- pow(1/sig_N[r,a], 2)
        sig_N[r,a] ~ dexp(1E-4)
      }
    }
  
    # ------------------------------------------
    # HARVEST ON THE CHENA AND SALCHA #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){
        H_2[y,r] ~ dnorm(mu_2[r], tau_2[r])T(0, N_2_dot[y,r])
      }
      mu_2[r] ~ dunif(0, 2000)
      tau_2[r] <- pow(1/sig_2[r], 2)
      sig_2[r] ~ dexp(1E-4)
    }
  
    # ------------------------------------------
    # SPAWNERS GIVEN IN-RIVER-RUN ABUNDANCE AND HARVEST ON THE CHENA AND SALCHA #
    # ------------------------------------------
    for (r in 1:2){
      for (a in 1:6){
        for (y in 1:n_years){                               
          S_a[y,r,a] <- max(N_2[y,r,a]-p_2[y,r,a]*H_2[y,r], 0.0001)
        }
      }
    }
    
    # ------------------------------------------
    # AGE PROPORTION OF HARVEST ON CHENA AND SALCHA  #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){
        p_2[y,r,1:6] ~ ddirch(gamma_2[r,1:6])
      }
      for (a in 1:6){
        gamma_2[r,a]~ dexp(0.0001)
      }
    }  
  
    # ------------------------------------------
    # TOTAL NUMBER OF SPAWNERS
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){                               
        S[y,r] <- sum(S_a[y,r,1:6]) 
      }
    }
    
    # ------------------------------------------
    # RICKER SR PROCESS WITH AN AR(1) TERM #
    # ------------------------------------------
    for (r in 1:2){
      log_R[1,r] ~ dnorm(mu_sr[1,r], tau_w[r])
      mu_sr[1,r] <- log(alpha[r]) + log(S[1,r]) - beta[r]*S[1,r]
      nu[1, r] <- 0
      R[1,r] <- exp(log_R[1,r])
      for (y in 2:n_years){
        log_R[y,r] ~ dnorm(mu_sr[y,r], tau_w[r])
        mu_sr[y,r] <- log(alpha[r]) + log(S[y,r]) - beta[r]*S[y,r] + phi[r]*nu[y-1,r]
        nu[y, r] <- log_R[y,r]-log(alpha[r])-log(S[y,r])+beta[r]*S[y,r]
        R[y,r] <- exp(log_R[y,r])
      }
      tau_w[r] <- pow(1/sig_w[r], 2)
      sig_w[r] ~ dexp(0.1)
      alpha[r] ~ dexp(1E-2)T(1,)
      log_alpha[r] <- log(alpha[r])
      phi[r] ~ dunif(-1,1)
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
    # TIME VARYING AGE AT MATURITY # 
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
  
    # ------------------------------------------
    # IRRA GIVEN RETURNERS AND MIDDLE YUKON HARVEST #
    # ------------------------------------------
    for (r in 1:2){
      for (a in 1:6){
        for (y in (n_ages+1):n_years){
          N_2[y,r,a] <- max(N_1[y,r,a]-q[y,r]*p_1[y,a]*H_1[y], 0.0001) 
        }
      }
    }
    
    # ------------------------------------------
    # TOTAL IN-RIVER RUN ABUNDANCE #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_years){
        N_2_dot[y,r] <- sum(N_2[y,r,1:6])
      }
    }
    
    
    # ------------------------------------------
    # PROBABILITY OF MOVEMENT TO CHENA/SALCHA FROM MIDDLE YUKON #
    # ------------------------------------------
    for (r in 1:2){
      for (y in 1:n_ages){
        q[y,r] ~ dnorm(mu_q[r], tau_q[r])
      }
      for (y in (n_ages+1):n_years){
        q[y,r] ~ dnorm(mu_q[r], tau_q[r])T(0, min(1,min(N_1[y,r,1:6])/H_1[y]))
      }
      mu_q[r] ~ dunif(0,1)
      tau_q[r] <- pow(1/sig_q[r],2)
      sig_q[r] ~ dexp(0.001)
    }
    
    # ------------------------------------------
    # AGE PROPORTION OF HARVEST ON MIDDLE YUKON  #
    # ------------------------------------------
    for (y in 1:n_years){
        p_1[y,1:6] ~ ddirch(gamma_1[1:6])
    }
    for (a in 1:6){
      gamma_1[a]~ dexp(0.0001)
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
      
      log_N_hat_mr[y, 1] ~ dnorm(log(N_2_dot[y,1]) - delta[y], tau_mr[y,1])
      log_N_hat_mr[y, 2] ~ dnorm(log(N_2_dot[y,2]), tau_mr[y,2])
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
        
        log_N_hat_tow[y, r] ~ dnorm(log(N_2_dot[y,r]), tau_tow[y,r])
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
    }
    
    
    
    
    for (y in (n_ages+1):n_years){
    
      # ------------------------------------------
      # AGE DATA FROM MIDDLE YUKON #
      # ------------------------------------------
        
      N_hat_pm[y, 1:6] ~ dmulti(
        c(p_1[y-3,1], p_1[y-4,2], p_1[y-5,3], p_1[y-6,4], p_1[y-7,5], p_1[y-8,6]),
        N_hat_pm_dot[y]
      )
        
      for(r in 1:2){
        
        # ------------------------------------------
        # AGE DATA FROM THE CHENA AND SALCHA #
        # ------------------------------------------
      
        N_hat_pr[y, r, 1:6] ~ dmulti(
          c(p_2[y-3,r,1], p_2[y-4,r,2], p_2[y-5,r,3], p_2[y-6,r,4], p_2[y-7,r,5], p_2[y-8,r,6]),
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

    # --------------
    # TOGGLE THE COMMENTS ACCORDING TO THE RS PROCESS 
  
    for (r in 1:2){
  
      # --------------
      # WITH THE AR(1) TERM #
      alpha_prime[r] <- alpha[r]*exp(pow(sig_w[r], 2)/(2*(1-pow(phi[r], 2)))) # LOG-NORMAL AND AR(1) SERIAL CORRELATION CORRECTION #
  
      # --------------
      # FOR THE RICKER RS RELATIONSHIP WITHOUT TIME VARYING PRODUCTIVITY #
      S_msy[r] <- log(alpha_prime[r])/beta[r]*(0.5-0.07*log(alpha_prime[r]))
      R_msy[r] <- alpha_prime[r]*S_msy[r]*exp(-beta[r]*S_msy[r])
      MSY[r] <- R_msy[r]-S_msy[r]
      S_max[r] <- 1/beta[r]
      MSR[r] <- alpha_prime[r]*S_max[r]*exp(-beta[r]*S_max[r])
      S_eq[r] <- log(alpha_prime[r])/beta[r]
      U_msy[r] <- log(alpha_prime[r])*(0.5-0.07*log(alpha_prime[r]))
   
    }

    ###################################################################################################################################  
    ############################  OPTIMAL YIELD, OVERFISHING, AND OPTIMAL RECRUITMENT PROBABILITY PROFILES ############################ 
    ###################################################################################################################################  
  
    for (i in 1:450){
      S_star[i] <- 50*i
      for (r in 1:2){
        R_star[i,r] <- alpha_prime[r]*S_star[i]*exp(-beta[r]*S_star[i])
        SY[i,r] <- R_star[i,r]-S_star[i]
    
        # FOR OPTIMAL YIELD AND OVERFISHING PROFILES #
        I_90_1[i,r] <- step(SY[i,r]-0.9*MSY[r])
        I_80_1[i,r] <- step(SY[i,r]-0.8*MSY[r])
        I_70_1[i,r] <- step(SY[i,r]-0.7*MSY[r])
    
        # FOR OPTIMAL RECRUITMENT PROFILE #
        I_90_2[i,r] <- step(R_star[i,r]-0.9*MSR[r])
        I_80_2[i,r] <- step(R_star[i,r]-0.8*MSR[r])
        I_70_2[i,r] <- step(R_star[i,r]-0.7*MSR[r])

      }
    }
  }"
  
  writeLines(mod,con=path)
}
