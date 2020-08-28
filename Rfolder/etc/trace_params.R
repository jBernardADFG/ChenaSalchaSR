#' Get desired parameters for traceplots
#' @param params (character vector) output of get_params
#' @export
trace_params <- function(params){
  params[!is.element(params, c("SY", "I_90_1", "I_80_1", "I_70_1", "I_90_2", "I_80_2", "I_70_2"))]
}

