#' Save useful model run info #
#' @param model_specs (list) Named list specifying the run specifications.
#' @param run_time (difftime) The time it took for the model to run.
#' @param DIC (numeric) DIC reported by JAGS.
#' @param file_path (character) File path to save model run information.
#' @param exists (logical) If a model run information table already exists, use exists=T. Use exists=F otherwise.
#' @export
save_run_info <- function(model_specs, run_time, DIC, pD, file_path="Tables/run_info.xlsx", exists=T){
  DIC <- jags_out$DIC
  pD <- jags_out$pD
  DIC <- round(DIC, 2)
  pD <- round(pD, 2)
  run_time <- paste(as.numeric(round(as.numeric(run_time))), units(run_time))
  run_dat <- data.frame(c(model_specs, run_time, DIC, pD))
  names(run_dat) <- c("run name",
                      "notes",
                      "n chains",
                      "n iterations",
                      "n burnin",
                      "n_thin",
                      "parallel",
                      "time to run",
                      "DIC",
                      "pD")
  run_dat <- run_dat[,c(1,3:10,2)]
  if (!exists){
    writexl::write_xlsx(run_dat, path=file_path)
  }else{
    file_path="Tables/run_info.xlsx"
    old_data <- readxl::read_xlsx(path=file_path)
    all_data <- rbind(old_data, run_dat)
    writexl::write_xlsx(all_data, path=file_path)
  }
}
