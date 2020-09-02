#' Plot residuals of a model -- need to implement for all models, but should work for base model
#' @param samples (data.frame) output of clean_chains.
#' @param file_path (character) file path to store the output jpeg file.
#' @param width (numeric) the width of the output jpeg file (in px).
#' @param height (numeric) the height of the output jpeg file (in px).
#' @export
residual_plot <- function(samples, file_path, width=750, height=500){
  nus <- samples[,substr(names(samples), 1, 2)=="nu"]
  nu_c <- nus[,1:(ncol(nus)/2)]
  nu_s <- nus[,(ncol(nus)/2):ncol(nus)]
  med_nu_c <- apply(nu_c, MARGIN=2, median)
  med_nu_s <- apply(nu_s, MARGIN=2, median)
  years <- 33
  med_nu_c <- med_nu_c[1:years]
  med_nu_s <- med_nu_s[1:years]
  year <- 1986:(1985+years)
  jpeg(filename=file_path, width=width, height=height, units="px")
  par(mfrow=c(1,2))
  plot(year, med_nu_c, ty="l", main="Chena", ylab="residual error")
  points(year, med_nu_c, col="black", pch=19, cex=2)
  points(year, med_nu_c, col="red", pch=19, cex=1)
  abline(lm(med_nu_c~year), col="blue", lwd=3)
  plot(year, med_nu_s, ty="l", main="Salcha", ylab="residual error")
  points(year, med_nu_s, col="black", pch=19, cex=2)
  points(year, med_nu_s, col="red", pch=19, cex=1)
  abline(lm(med_nu_s~year), col="blue", lwd=3)
  dev.off()
}  

