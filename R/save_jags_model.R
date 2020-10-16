#' Save .jags version of a JAGS module
#' Modified JAGS modules need saved for the changes to take effect. This function streamlines the process. Jags modules are base.R, base_ar.R, base_tvm.R, base_tvm_ar.R, base_tvp.R, base_wl.R, and bev_holt.R
#' @param mod_path (character) path to save the .jags file
#' @param model (character) the model to save. accepted values are "base", "base_tvm", "base_ar", "base_tvm_ar", "base_tvp", "base_wl", and "bev_holt"
#' @export
save_jags_model <- function(mod_path, model){
  if (model=="base"){
    write_jags_model.base(mod_path)
  }
  if (model=="base_tvm"){
    write_jags_model.base_tvm(mod_path)
  }
  if (model=="base_ar"){
    write_jags_model.base_ar(mod_path)
  }
  if (model=="base_tvm_ar"){
    write_jags_model.base_tvm_ar(mod_path)
  }
  if (model=="base_tvp"){
    write_jags_model.base_tvp(mod_path)
  }
  if (model == "base_tvm_tvp"){
    write_jags_model.base_tvm_tvp(mod_path)
  }
  if (model == "base_tvm_ar_tvp"){
    write_jags_model.base_tvm_ar_tvp(mod_path)
  }
  if (model=="base_ld"){
    write_jags_model.base_ld(mod_path)
  }
  if (model=="base_tvp_ld"){
    write_jags_model.base_tvp_ld(mod_path)
  }
  if (model=="base_tvm_ar_ash"){
    write_jags_model.base_tvm_ash(mod_path)
  }
}
