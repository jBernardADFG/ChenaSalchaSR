#' Base Ricker with time varying productivity
#' @param path (character) path to write .jags file
#' @export
write_jags_model.base_tvp_ld <- function(path){
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

    # --------------
    # RICKER RS PROCESS WITH A TIME VARYING PRODUCTIVITY PARAMETER AND A LINEAR CONSTRAINT ON THE PRODUCTIVITY PARAMETER #
    for (r in 1:2){
      for (y in 1:n_years){
        log_R[y,r] ~ dnorm(mu_sr[y,r], tau_w[r])
        mu_sr[y,r] <- ifelse(
          S[y,r] <= S_crit[y,r],
          log(a_0[r]+a_1[r]*(y-1))-log(2*S_crit[y,r])+2*log(S[y,r]),
          log((a_0[r]+a_1[r]*(y-1))*(max(S[y,r]-S_crit[y,r], 0.01))*exp(-beta[r]*(max(S[y,r]-S_crit[y,r], 0.01))) + (a_0[r]+a_1[r]*(y-1))/2*S_crit[y,r])
        )
        alpha[y,r] <- a_0[r]+a_1[r]*(y-1)
        log_alpha[y,r] <- log(alpha[y,r])
        R[y,r] <- exp(log_R[y,r])
        nu[y,r] <- log_R[y,r]-log(alpha[y,r])-log(S[y,r])+beta[r]*S[y,r]
        S_crit[y,r] ~ dunif(0, 10000)
      }
      tau_w[r] <- pow(1/sig_w[r], 2)
      sig_w[r] ~ dexp(0.1)
      a_0[r] ~ dunif(1, 20)
      a_1[r] ~ dunif((1-a_0[r])/(n_years-1), 5)
      beta[r] ~ dexp(1E2)
    }

    # ------------------------------------------
    # RETURNERS GIVEN RECRUITS #
    # ------------------------------------------
    for (r in 1:2){
      for (y in (n_ages+1):n_years){
        for (a in 1:6){
          N_1[y,r,a] <- R[(y-9+a),r]*p[y,r,7-a]
        }
        N_1_dot[y,r] <- sum(N_1[y,r,1:6])
      }
    }

    # ------------------------------------------
    # Age-at-maturity probability vector  
    # ------------------------------------------
    # ----------------
    # WITHOUT TIME VARYING AGE-AT-MATURITY #
    for (r in 1:2){
      for (y in 1:n_years){
        p[y,r,1:6] ~ ddirch(gamma[r,1:6]+0.1)
      }
    }
    for (r in 1:2){
      for (a in 1:n_ages){
        gamma[r,a] ~ dexp(0.1)
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
      
        # ------------------------------------------
        # AGE DATA FROM THE CHENA AND SALCHA #
        # ------------------------------------------
        N_hat_pr[y, r, 1:6] ~ dmulti(p[y,r,1:6], N_hat_pr_dot[y,r])
  
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
      for (y in 1:n_years){
        # --------------
        # TIME VARYING PRODUCTIVITY WITHOUT THE AR(1) TERM #
        alpha_prime[y,r] <- alpha[y,r]*exp(pow(sig_w[r], 2)/2)
      }
    }  
    for (i in 1:450){
      S_star[i] <- 50*i
      for (r in 1:2){
        for (y in 1:n_years){
          R_star[i,y,r] <- ifelse(
            S_star[i] <= S_crit[y,r],
            alpha_prime[y,r]/(2*S_crit[y,r])*S_star[i]^2,
            alpha_prime[y,r]*(max(S_star[i]-S_crit[y,r], 0.01))*exp(-beta[r]*(max(S_star[i]-S_crit[y,r], 0.01))) + alpha_prime[y,r]/2*S_crit[y,r]
          )
          SY[i,y,r] <- R_star[i,y,r]-S_star[i]
        }
      }
    }
    
  }"
  writeLines(mod,con=path)
}
