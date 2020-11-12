# CLEAR GLOBAL ENVIRONMENT #
rm(list=objects())

# INSTALL PROJECT PACKAGE -- once the package has been installed, you do not need to rerun this line # 
devtools::install_github("jBernardADFG/ChenaSalchaSR", force=T)

# LOAD PROJECT PACKAGE #
library(ChenaSalchaSR)

# WHICH MODEL IS BEING RUN? -- run one of the following lines of code #
model <- "base" # basic Ricker model
model <- "base_tvm" # basic ricker model + time varying age-at-maturity 
model <- "base_ar" # basic ricker model + AR(1) term
model <- "base_tvm_ar" # basic ricker model + time varying age-at-maturity + AR(1) term
model <- "base_tvp" # basic ricker model + time varying productivity term 

# Fun Ideas I'm still working on #
model <- "base_tvm_ar_ash" # basic Ricker model +  time varying age-at-maturity + AR(1) term + age-stratified-harvest term
model <- "base_tvm_ar_ld" # low density Ricker + time varying age-at-maturity + AR(1) term
model <- "base_tvm_ar_wl" # basic Ricker model + time varying age-at-maturity + AR(1) term + water-level term
model <- "base_tvm_ar_tvp" # basic Ricker model + time varying age-at-maturity + time varying productivity

# READ IN DATA AND FORMAT FOR USE IN JAGS MODEL #
{
  setwd("S:/Jordy/ChenaSalchaSR/R/WorkingDirectory")
  data <- get_jags_data("Data/2020-Data-Median.xlsx", model)
  # data <- get_jags_data("Data/2020-Data-Max.xlsx") # To use different harvest SEs for sensitivity testing
}

# GET FILE PATH TO .JAGS FILE #
mod_path <- paste("Jags/", model, ".jags", sep="")

# SAVE JAGS MODULE AS .JAGS FILE -- you should only run this line if modifications have been made to a .Jags file #
save_jags_model(mod_path, model)

# GET PARAMETERS TO MONITOR #
params <- get_params(model)

# SET MODEL SPECIFICATIONS #
model_specs <- set_model_specifications(
  run_name = "final_base_tvm_ar_1",
  notes = "final_base_tvm_ar_1",
  n_chains = 6,
  n_iter = 7000000, # 3500000 
  n_burnin = 3500000, # 250000
  n_thin = 2000, # 1000
  parallel = F
)

# RUN JAGS MODEL #
# Note: This is a model that will not run in 4 hours
# To get the thing to converge, the program picks initial values for alpha and beta (the estimates reported in the last Chena/Salcha report are used)
# The MCMC routine is then run for 3500000 iterations
# Once this is done, the end values of the chains are used as initial values for the real run which should be run for at least 3500000 iterations
# You're probably looking 3-4 days of computation time if you want |R^2| < 1.1 for all parameters
{
  start_time <- Sys.time()
  jags_out <- jagsUI::jags(
    data = data,
    parameters.to.save = params,
    model.file = mod_path,
    n.chains = model_specs$n_chains,
    n.iter = model_specs$n_burnin,
    n.burnin = 1,
    n.thin = model_specs$n_thin,
    parallel = model_specs$parallel,
    inits=get_initial_values(model)
  )
  run_time <- Sys.time()-start_time
  start_time <- Sys.time()
  jags_out <- jagsUI::jags(data = data,
                           parameters.to.save = params,
                           model.file = mod_path,
                           n.chains = model_specs$n_chains,
                           n.iter = model_specs$n_iter,
                           n.burnin = 1,
                           n.thin = model_specs$n_thin,
                           parallel = model_specs$parallel,
                           inits=get_new_inits(jags_out)
  )
  run_time <- Sys.time()-start_time
}

# SAVE CHAINS AND USEFUL INFO RELATED TO MODEL RUN #
{
  save(jags_out, file=paste("Rdata/", model_specs$run_name, ".Rdata", sep=""))
  save_run_info(model_specs, run_time, jags_out, file_path="Tables/run_info.xlsx")
}

# CONVERGENCE DIAGNOSTICS
summary(jags_out)
MCMCvis::MCMCtrace(jags_out,
                   params = trace_params(params),
                   filename = paste("Traceplots/", model_specs$run_name, ".pdf", sep=""),
                   open_pdf = F)

# CONVERT CHAINS TO AN EASIER TO USE FORMAT AND DISCARD INITIAL SAMPLES IF THE BURNIN PERIOD WAS NOT LONG ENOUGH
samples <- clean_chains(jags_out)

# CREATE TABLES #
{
  extract_abundance_estimates(samples, 
                              alpha=0.10, 
                              file_path = paste("Tables/abundance_estimates/", model_specs$run_name, ".xlsx", sep=""),
                              final_year = 2030)
  
  extract_abundance_by_age(samples, 
                           alpha=0.10, 
                           file_path=paste("Tables/abundance_by_age/", model_specs$run_name, ".xlsx", sep=""),
                           final_year=2030)
  
  extract_age_at_maturity(samples, 
                          alpha=0.10, 
                          file_path=paste("Tables/age_at_maturity/", model_specs$run_name, ".xlsx", sep=""),
                          final_year=2030)
  
  extract_sr_params(samples, 
                    alpha=0.10,
                    model=model,
                    file_path=paste("Tables/sr_params/", model_specs$run_name, ".xlsx", sep=""))
}

# CREATE PLOTS #
{
  age_hist(samples, 
           file_path=paste("Plots/age_hists/", model_specs$run_name, ".jpeg", sep=""),
           final_year=2030)
  
  horsetail_plot(samples,
                 model,
                 n_draws=50,
                 sig_lev = 0.50,
                 r_up=c(35000, 65000),
                 file_path=paste("Plots/horsetail/", model_specs$run_name, ".jpeg", sep=""))
  
  sy_plot(samples,
          model,
          y_up=80000,
          file_path=paste("Plots/expected_yield/", model_specs$run_name, ".jpeg", sep=""))
  
  profile_plot(samples,
               model,
               old_goal_med=c(4000, 5500), old_goal_lo=c(3000, 4000), old_goal_up=c(5000, 7000),
               file_path=paste("Plots/profiles/", model_specs$run_name, ".jpeg", sep=""))
  
  residual_plot(samples,
                file_path=paste("Plots/residuals/", model_specs$run_name, ".jpeg", sep=""))
}
