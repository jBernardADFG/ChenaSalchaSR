#' Retrieve model specific parameters to monitor
#' @param model (character) the model that is being run. Accepted values are "base", "base_tvm", "base_ar", and "base_tvm_ar".
#' @export

get_params <- function(model){
  if (model == "base"){
    params <- c("alpha", "log_alpha", "beta", "sig_w", "nu",
                "mu_N", "sig_N", "mu_2", "sig_2", "mu_1", "sig_1", "lambda", "mu_q", "sig_q",
                "R", "S", "N_1", "N_1_dot", "N_2", "p",
                "S_msy", "R_msy", "MSY", "S_max", "MSR", "S_eq", "U_msy",
                "SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2")
  }
  if (model=="base_tvm"){
    params <- c("alpha", "log_alpha", "beta", "sig_w", "nu",
                "mu_N", "sig_N", "mu_2", "sig_2", "D", "n", "mu_1", "sig_1", "lambda", "mu_q", "sig_q",
                "R", "S", "N_1", "N_1_dot", "N_2", "p",
                "S_msy", "R_msy", "MSY", "S_max", "MSR", "S_eq", "U_msy",
                "SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2")
  }
  if (model=="base_ar"){
    params <- c("alpha", "log_alpha", "beta", "phi", "sig_w", "nu",
                "mu_N", "sig_N", "mu_2", "sig_2", "mu_1", "sig_1", "lambda", "mu_q", "sig_q",
                "R", "S", "N_1", "N_1_dot", "N_2", "p",
                "S_msy", "R_msy", "MSY", "S_max", "MSR", "S_eq", "U_msy",
                "SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2")
  }
  if (model=="base_tvm_ar"){
    params <- c("alpha", "log_alpha", "beta", "phi", "sig_w", "nu",
                "mu_N", "sig_N", "mu_2", "sig_2", "D", "n", "mu_1", "sig_1", "lambda", "mu_q", "sig_q",
                "R", "S", "N_1", "N_1_dot", "N_2", "p",
                "S_msy", "R_msy", "MSY", "S_max", "MSR", "S_eq", "U_msy",
                "SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2")
  }
  if (model=="base_tvp"){
    params <- c("alpha", "log_alpha", "a_1", "a_2", "beta", "sig_w", "nu",
                "mu_N", "sig_N", "mu_2", "sig_2", "mu_1", "sig_1", "lambda", "mu_q", "sig_q",
                "R", "S", "N_1", "N_1_dot", "N_2", "p",
                "S_msy", "R_msy", "MSY", "S_max", "MSR", "S_eq", "U_msy",
                "SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2")
  }
  return(params)
}
