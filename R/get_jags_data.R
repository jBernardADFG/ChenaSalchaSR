#' Read in and format data for JAGS
#' @param file_path (character) Directory to the xlsx data. Note: the format of the excel table needs to be standardized (see Data/2020-Data.xlsx).
#' @param model (character) The model being run.
#' @export

get_jags_data <- function(file_path, model){
  type="Paired"
  if (!is.element(type, c("Paired","Chena", "Salcha"))){
    stop("type must be Paired, Chena, or Salcha")
  }
  my_data <- readxl::read_xlsx(file_path, sheet="Middle Yukon", skip=2)
  if (type=="Paired"){
    chena_data <- suppressMessages(readxl::read_xlsx(file_path, sheet="Chena", skip=2))
    salcha_data <- suppressMessages(readxl::read_xlsx(file_path, sheet="Salcha", skip=2))
    data <- list(chena_data=chena_data, salcha_data=salcha_data, my_data=my_data)
    attr(data, "class") <- "paired"
  }
  if (type=="Chena"){
    chena_data <- suppressMessages(readxl::read_xlsx(file_path, sheet="Chena", skip=2))
    data <- list(river_data=chena_data, my_data=my_data)
    attr(data, "class") <- "standalone"
    attr(data, "river") <- "Chena"
  }
  if (type=="Salcha"){
    salcha_data <- suppressMessages(readxl::read_xlsx(file_path, sheet="Salcha", skip=2))
    data <- list(river_data=salcha_data, my_data=my_data)
    attr(data, "class") <- "standalone"
    attr(data, "river") <- "Salcha"
  }
  jags_data <- format(data)
  jags_data$log_N_hat_mr <- as.matrix(jags_data$log_N_hat_mr)
  jags_data$log_N_hat_tow <- as.matrix(jags_data$log_N_hat_tow)
  jags_data$mr_cv <- as.matrix(jags_data$mr_cv)
  jags_data$tow_cv <- as.matrix(jags_data$tow_cv)
  jags_data$H_hat_2 <- as.matrix(jags_data$H_hat_2)
  jags_data$se_H_hat_2 <- as.matrix(jags_data$se_H_hat_2)
  jags_data$N_hat_q <- as.matrix(jags_data$N_hat_q)
  
  if (model != "base_tvm_ar_ash"){
    jags_data <- jags_data[names(jags_data) %in% c("N_hat_pm", "N_hat_pm_dot") == FALSE]
  }else{
    jags_data$N_hat_pm <- jags_data$N_hat_pm[,1,]
    jags_data$N_hat_pm_dot <- jags_data$N_hat_pm_dot[,1]
  }


  
  return(jags_data)
}
