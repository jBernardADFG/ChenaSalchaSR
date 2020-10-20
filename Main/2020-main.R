# TO DO #
# SOME RESIDUAL DIAGNOSTICS -- WRITE FUNCTION TO GENERATE PLOT -- DOCUMENT DECREASES IN PRODUCTIVITY #
# PREDICTED ALLOWABLE HARVEST #
# MIGHT BE INTERESTING TO RELATE TO SIZE AND AGE-AT-MATURITY #

# CLEAR GLOBAL ENVIRONMENT
rm(list=objects())

# INSTALL PROJECT PACKAGE -- once the package has been installed, you do not need to rerun this line 
devtools::install_github("jBernardADFG/ChenaSalchaSR", force=T)

# LOAD PROJECT PACKAGE
library(ChenaSalchaSR)

# WHICH MODEL IS BEING RUN ? 
# choose one of the following options:
model <- "base" # basic Ricker model
model <- "base_tvm" # time varying age-at-maturity added to base Ricker 
model <- "base_ar" # AR(1) term added to base Ricker --
model <- "base_tvm_ar" # time varying age-at-maturity and AR(1) term added to base Ricker -- this is basically what was used on the Copper
model <- "base_tvp" # time varying productivity term added to base Ricker
model <- "base_tvm_tvp" # time varying maturity and time varying productivity added to base Ricker  
model <- "base_tvm_ar_tvp" # time varying age-at-maturity, AR(1), and time varying productivity terms added to base Ricker

# still working on implementing these:
model <- "base_tvm_ar_ash" # time varying age-at-maturity, AR(1), and age-stratified-harvest terms added to base Ricker
model <- "base_tvm_ar_ld" # low-density Ricker curve with time-varying productivity and AR(1) terms
model <- "base_tvm_ar_wl" # time varying age-at-maturity, AR(1), and water level terms added to base Ricker

# READ IN DATA AND FORMAT FOR USE IN JAGS MODEL
{
  setwd("S:/Jordy/ChenaSalchaSR/R/WorkingDirectory")
  data <- get_jags_data("Data/2020-Data-Median.xlsx", model)
  # data <- get_jags_data("Data/2020-Data-Max.xlsx") # to use different harvest SE's for sensitivity testing
}

# GET .JAGS FILE PATH
mod_path <- paste("Jags/", model, ".jags", sep="")

# SAVE JAGS MODULE AS .JAGS FILE -- only needs to be run when modifications have been made to a jags module
save_jags_model(mod_path, model)

# PARAMETERS TO MONITOR
params <- get_params(model)     #####

# SET MODEL SPECIFICATIONS
model_specs <- set_model_specifications(
  run_name = "base_tvm_ar_tvp final test",
  notes = "preparing base_tvm_ar_tvp model for final cook",
  n_chains = 4,
  n_iter = 5000, #500000 #3500000 
  n_burnin = 2500, #250000 #250000
  n_thin = 1, #500 #1000
  parallel = F
)

# RUN JAGS MODEL
{
  start_time <- Sys.time()
  jags_out <- jagsUI::jags(data = data,
                          parameters.to.save = params,
                          model.file = mod_path,
                          n.chains = model_specs$n_chains,
                          n.iter = model_specs$n_iter,
                          n.burnin = model_specs$n_burnin,
                          n.thin = model_specs$n_thin,
                          parallel = model_specs$parallel,
                          inits=get_initial_values(model)
                          )
  run_time <- Sys.time()-start_time
}

# SAVE CHAINS AND USEFUL INFO RELATED TO MODEL RUN
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

# CREATE TABLES
{
  extract_abundance_estimates(samples, 
                              alpha=0.10, 
                              file_path = paste("Tables/abundance_estimates/", model_specs$run_name, ".xlsx", sep=""),
                              final_year = 2030) # Looks good
  
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

# CREATE PLOTS
{
  age_hist(samples, 
           file_path=paste("Plots/age_hists/", model_specs$run_name, ".jpeg", sep=""),
           final_year=2030)
  
  horsetail_plot(samples,
                 model,
                 n_draws=50,
                 sig_lev = 0.1,
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
