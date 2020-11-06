#' Escapement by Age Histogram
#' @param samples (data.frame) output of clean_chains
#' @param file_path  (character) file path to store output jpeg file.
#' @param width (numeric) width of the output plot (in px).
#' @param height (numeric) height of the output plot (in px).
#' @export
age_hist <- function(samples, file_path, width=500, height=1000, final_year=2025){
  
  #
  file_path=paste("Plots/age_hists/", model_specs$run_name, ".jpeg", sep="")
  final_year=2025
  width=500
  height=1000
  #
  
  
  eb_samples_c <- samples[,substr(names(samples), 1, 7)=="N_1_dot" & substr(names(samples), nchar(names(samples))-1, nchar(names(samples)))=="1]"]
  eb_c <- apply(eb_samples_c, MARGIN=2, quantile, probs = c(0.25, 0.75))
  eb_lo_c <- as.numeric(eb_c[1,]) 
  eb_hi_c <- as.numeric(eb_c[2,])
  eb_c <- data.frame(lo = eb_lo_c, hi=eb_hi_c)
  
  eb_samples_s <- samples[,substr(names(samples), 1, 7)=="N_1_dot" & substr(names(samples), nchar(names(samples))-1, nchar(names(samples)))=="2]"]
  eb_s <- apply(eb_samples_s, MARGIN=2, quantile, probs = c(0.25, 0.75))
  eb_lo_s <- as.numeric(eb_s[1,]) 
  eb_hi_s <- as.numeric(eb_s[2,])
  
  # file_path=paste("Plots/age_hists/", model_specs$run_name, ".jpeg", sep="")
  # final_year=2030
  # width=500
  # height=1000
  # final_year=2020
  
  aba_samples <- samples[,substr(names(samples), 1, 3)=="N_1" &
                           substr(names(samples), 4, 4)!="_"]
  quants <- apply(aba_samples, MARGIN=2, quantile, probs = c(0.5))
  tot_years <- length(1994:2030)
  n_years <- length(1994:final_year)
  med_array <- array(NA, dim=c(tot_years, 2, 6))
  l <- 1
  for (k in 1:6){
    for (j in 1:2){
      for (i in 1:tot_years){
        med_array[i,j,k] <- quants[l]
        l <- l + 1
      }
    }
  }
  med_array <- med_array[1:n_years,,]
  chena_tab <- salcha_tab <- data.frame(
    year=1994:final_year,
    Age_3=rep(NA, n_years),
    Age_4=rep(NA, n_years),
    Age_5=rep(NA, n_years),
    Age_6=rep(NA, n_years),
    Age_7=rep(NA, n_years),
    Age_8=rep(NA, n_years)
  )
  for (i in 1:n_years){
    for (k in 1:6){
      chena_tab[i, k+1] <- med_array[i, 1, k]
      salcha_tab[i, k+1] <- med_array[i, 2, k]
    }
  }
  chena_n <- data.frame(year=rep(1994:final_year, rep(6, length(1994:final_year))),
                        Age=rep(1:6, length(1994:final_year)),
                        run=rep(NA, 6*length(1994:final_year)))
  salcha_n <- data.frame(year=rep(1994:final_year, rep(6, length(1994:final_year))),
                         Age=rep(1:6, length(1994:final_year)),
                         run=rep(NA, 6*length(1994:final_year)))
  k <- 1
  for(i in 1:nrow(chena_tab)){
    for(j in 2:ncol(chena_tab)){
      chena_n[k,3] <- chena_tab[i,j]
      salcha_n[k,3] <- salcha_tab[i,j]
      k<- k+1
    }
  }
  aam_samples <- samples[,substr(names(samples), 1, 1) == "p" &
                           substr(names(samples), 2, 2) != "h"]
  
  quants <- apply(aam_samples, MARGIN=2, quantile, probs = c(0.5))
  
  tot_years <- length(1986:2030)
  med_array <- array(NA, dim=c(tot_years, 2, 6))
  
  l <- 1
  for (k in 1:6){
    for (j in 1:2){
      if (j == 1){
        tot_years <- 45
      }
      else{
        tot_years <- 43
      }
      for (i in 1:tot_years){
        med_array[i,j,k] <- quants[l]
        l <- l + 1
      }
    }
  }
  med_array <- med_array[9:length(1986:final_year),,]
  chena_tab <- salcha_tab <- data.frame(
    year=1994:final_year,
    Age_3=rep(NA, n_years),
    Age_4=rep(NA, n_years),
    Age_5=rep(NA, n_years),
    Age_6=rep(NA, n_years),
    Age_7=rep(NA, n_years),
    Age_8=rep(NA, n_years)
  )
  for (i in 1:n_years){
    for (k in 1:6){
      chena_tab[i, k+1] <- med_array[i, 1, k]
      salcha_tab[i, k+1] <- med_array[i, 2, k]
    }
  }
  chena_tab
  salcha_tab
  chena_norm <- apply(chena_tab[,2:ncol(chena_tab)], MARGIN=1, sum)
  salcha_norm <- apply(salcha_tab[,2:ncol(salcha_tab)], MARGIN=1, sum)
  for(i in 1:nrow(chena_tab)){
    for(j in 2:ncol(chena_tab)){
      chena_tab[i,j] <- chena_tab[i,j]/chena_norm[i]
      salcha_tab[i,j] <- salcha_tab[i,j]/salcha_norm[i]
    }
  }
  
  chena_p <- data.frame(year=rep(1994:final_year, rep(6, n_years)),
                        Age=rep(1:6, n_years),
                        run=rep(NA, 6*n_years))
  salcha_p <- data.frame(year=rep(1994:final_year, rep(6, n_years)),
                         Age=rep(1:6, n_years),
                         run=rep(NA, 6*n_years))
  k <- 1
  for(i in 1:nrow(chena_tab)){
    for(j in 2:ncol(chena_tab)){
      chena_p[k,3] <- chena_tab[i,j]
      salcha_p[k,3] <- salcha_tab[i,j]
      k<- k+1
    }
  }
  chena_n$Age <- chena_n$Age + 2 
  salcha_n$Age <- salcha_n$Age + 2 
  chena_p$Age <- chena_p$Age + 2 
  salcha_p$Age <- salcha_p$Age + 2
  jpeg(filename = file_path,  width=width, height=height, unit="px")
  library(ggplot2)
  p1 <- ggplot2::ggplot(chena_n, aes(x=year, y=run, fill=Age))+
    geom_col()+
    theme_classic()+
    ggtitle("Chena River -- Total Run by Age") + ylab("Number of Chinook") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks=1994:final_year)+
    geom_segment(ggplot2::aes(x=1994, y=eb_lo_c[1], yend=eb_hi_c[1], xend=1994), color="red") +
    geom_segment(ggplot2::aes(x=1995, y=eb_lo_c[2], yend=eb_hi_c[2], xend=1995), color="red") +
    geom_segment(ggplot2::aes(x=1996, y=eb_lo_c[3], yend=eb_hi_c[3], xend=1996), color="red") +
    geom_segment(ggplot2::aes(x=1997, y=eb_lo_c[4], yend=eb_hi_c[4], xend=1997), color="red") + 
    geom_segment(ggplot2::aes(x=1998, y=eb_lo_c[5], yend=eb_hi_c[5], xend=1998), color="red") +
    geom_segment(ggplot2::aes(x=1999, y=eb_lo_c[6], yend=eb_hi_c[6], xend=1999), color="red") +
    geom_segment(ggplot2::aes(x=2000, y=eb_lo_c[7], yend=eb_hi_c[7], xend=2000), color="red") +
    geom_segment(ggplot2::aes(x=2001, y=eb_lo_c[8], yend=eb_hi_c[8], xend=2001), color="red") +
    geom_segment(ggplot2::aes(x=2002, y=eb_lo_c[9], yend=eb_hi_c[9], xend=2002), color="red") +
    geom_segment(ggplot2::aes(x=2003, y=eb_lo_c[10], yend=eb_hi_c[10], xend=2003), color="red") +
    geom_segment(ggplot2::aes(x=2004, y=eb_lo_c[11], yend=eb_hi_c[11], xend=2004), color="red") + 
    geom_segment(ggplot2::aes(x=2005, y=eb_lo_c[12], yend=eb_hi_c[12], xend=2005), color="red") +
    geom_segment(ggplot2::aes(x=2006, y=eb_lo_c[13], yend=eb_hi_c[13], xend=2006), color="red") +
    geom_segment(ggplot2::aes(x=2007, y=eb_lo_c[14], yend=eb_hi_c[14], xend=2007), color="red") +
    geom_segment(ggplot2::aes(x=2008, y=eb_lo_c[15], yend=eb_hi_c[15], xend=2008), color="red") +
    geom_segment(ggplot2::aes(x=2009, y=eb_lo_c[16], yend=eb_hi_c[16], xend=2009), color="red") +
    geom_segment(ggplot2::aes(x=2010, y=eb_lo_c[17], yend=eb_hi_c[17], xend=2010), color="red") +
    geom_segment(ggplot2::aes(x=2011, y=eb_lo_c[18], yend=eb_hi_c[18], xend=2011), color="red") +
    geom_segment(ggplot2::aes(x=2012, y=eb_lo_c[19], yend=eb_hi_c[19], xend=2012), color="red") +
    geom_segment(ggplot2::aes(x=2013, y=eb_lo_c[20], yend=eb_hi_c[20], xend=2013), color="red") +
    geom_segment(ggplot2::aes(x=2014, y=eb_lo_c[21], yend=eb_hi_c[21], xend=2014), color="red") +
    geom_segment(ggplot2::aes(x=2015, y=eb_lo_c[22], yend=eb_hi_c[22], xend=2015), color="red") +
    geom_segment(ggplot2::aes(x=2016, y=eb_lo_c[23], yend=eb_hi_c[23], xend=2016), color="red") +
    geom_segment(ggplot2::aes(x=2017, y=eb_lo_c[24], yend=eb_hi_c[24], xend=2017), color="red") +
    geom_segment(ggplot2::aes(x=2018, y=eb_lo_c[25], yend=eb_hi_c[25], xend=2018), color="red") +
    geom_segment(ggplot2::aes(x=2019, y=eb_lo_c[26], yend=eb_hi_c[26], xend=2019), color="red") +
    geom_segment(ggplot2::aes(x=2020, y=eb_lo_c[27], yend=eb_hi_c[27], xend=2020), color="red") +
    geom_segment(ggplot2::aes(x=2021, y=eb_lo_c[28], yend=eb_hi_c[28], xend=2021), color="red") +
    geom_segment(ggplot2::aes(x=2022, y=eb_lo_c[29], yend=eb_hi_c[29], xend=2022), color="red") +
    geom_segment(ggplot2::aes(x=2023, y=eb_lo_c[30], yend=eb_hi_c[30], xend=2023), color="red") +
    geom_segment(ggplot2::aes(x=2024, y=eb_lo_c[31], yend=eb_hi_c[31], xend=2024), color="red") +
    geom_segment(ggplot2::aes(x=2025, y=eb_lo_c[32], yend=eb_hi_c[32], xend=2025), color="red") +
    geom_segment(ggplot2::aes(x=1993.75, y=eb_lo_c[1], yend=eb_lo_c[1], xend=1994.25), color="red") +
    geom_segment(ggplot2::aes(x=1994.75, y=eb_lo_c[2], yend=eb_lo_c[2], xend=1995.25), color="red") +
    geom_segment(ggplot2::aes(x=1995.75, y=eb_lo_c[3], yend=eb_lo_c[3], xend=1996.25), color="red") +
    geom_segment(ggplot2::aes(x=1996.75, y=eb_lo_c[4], yend=eb_lo_c[4], xend=1997.25), color="red") + 
    geom_segment(ggplot2::aes(x=1997.75, y=eb_lo_c[5], yend=eb_lo_c[5], xend=1998.25), color="red") +
    geom_segment(ggplot2::aes(x=1998.75, y=eb_lo_c[6], yend=eb_lo_c[6], xend=1999.25), color="red") +
    geom_segment(ggplot2::aes(x=1999.75, y=eb_lo_c[7], yend=eb_lo_c[7], xend=2000.25), color="red") +
    geom_segment(ggplot2::aes(x=2000.75, y=eb_lo_c[8], yend=eb_lo_c[8], xend=2001.25), color="red") +
    geom_segment(ggplot2::aes(x=2001.75, y=eb_lo_c[9], yend=eb_lo_c[9], xend=2002.25), color="red") +
    geom_segment(ggplot2::aes(x=2002.75, y=eb_lo_c[10], yend=eb_lo_c[10], xend=2003.25), color="red") +
    geom_segment(ggplot2::aes(x=2003.75, y=eb_lo_c[11], yend=eb_lo_c[11], xend=2004.25), color="red") + 
    geom_segment(ggplot2::aes(x=2004.75, y=eb_lo_c[12], yend=eb_lo_c[12], xend=2005.25), color="red") +
    geom_segment(ggplot2::aes(x=2005.75, y=eb_lo_c[13], yend=eb_lo_c[13], xend=2006.25), color="red") +
    geom_segment(ggplot2::aes(x=2006.75, y=eb_lo_c[14], yend=eb_lo_c[14], xend=2007.25), color="red") +
    geom_segment(ggplot2::aes(x=2007.75, y=eb_lo_c[15], yend=eb_lo_c[15], xend=2008.25), color="red") +
    geom_segment(ggplot2::aes(x=2008.75, y=eb_lo_c[16], yend=eb_lo_c[16], xend=2009.25), color="red") +
    geom_segment(ggplot2::aes(x=2009.75, y=eb_lo_c[17], yend=eb_lo_c[17], xend=2010.25), color="red") +
    geom_segment(ggplot2::aes(x=2010.75, y=eb_lo_c[18], yend=eb_lo_c[18], xend=2011.25), color="red") +
    geom_segment(ggplot2::aes(x=2011.75, y=eb_lo_c[19], yend=eb_lo_c[19], xend=2012.25), color="red") +
    geom_segment(ggplot2::aes(x=2012.75, y=eb_lo_c[20], yend=eb_lo_c[20], xend=2013.25), color="red") +
    geom_segment(ggplot2::aes(x=2013.75, y=eb_lo_c[21], yend=eb_lo_c[21], xend=2014.25), color="red") +
    geom_segment(ggplot2::aes(x=2014.75, y=eb_lo_c[22], yend=eb_lo_c[22], xend=2015.25), color="red") +
    geom_segment(ggplot2::aes(x=2015.75, y=eb_lo_c[23], yend=eb_lo_c[23], xend=2016.25), color="red") +
    geom_segment(ggplot2::aes(x=2016.75, y=eb_lo_c[24], yend=eb_lo_c[24], xend=2017.25), color="red") +
    geom_segment(ggplot2::aes(x=2017.75, y=eb_lo_c[25], yend=eb_lo_c[25], xend=2018.25), color="red") +
    geom_segment(ggplot2::aes(x=2018.75, y=eb_lo_c[26], yend=eb_lo_c[26], xend=2019.25), color="red") +
    geom_segment(ggplot2::aes(x=2019.75, y=eb_lo_c[27], yend=eb_lo_c[27], xend=2020.25), color="red") +
    geom_segment(ggplot2::aes(x=2020.75, y=eb_lo_c[28], yend=eb_lo_c[28], xend=2021.25), color="red") +
    geom_segment(ggplot2::aes(x=2021.75, y=eb_lo_c[29], yend=eb_lo_c[29], xend=2022.25), color="red") +
    geom_segment(ggplot2::aes(x=2022.75, y=eb_lo_c[30], yend=eb_lo_c[30], xend=2023.25), color="red") +
    geom_segment(ggplot2::aes(x=2023.75, y=eb_lo_c[31], yend=eb_lo_c[31], xend=2024.25), color="red") +
    geom_segment(ggplot2::aes(x=2024.75, y=eb_lo_c[32], yend=eb_lo_c[32], xend=2025.25), color="red") +
    geom_segment(ggplot2::aes(x=1993.75, y=eb_hi_c[1], yend=eb_hi_c[1], xend=1994.25), color="red") +
    geom_segment(ggplot2::aes(x=1994.75, y=eb_hi_c[2], yend=eb_hi_c[2], xend=1995.25), color="red") +
    geom_segment(ggplot2::aes(x=1995.75, y=eb_hi_c[3], yend=eb_hi_c[3], xend=1996.25), color="red") +
    geom_segment(ggplot2::aes(x=1996.75, y=eb_hi_c[4], yend=eb_hi_c[4], xend=1997.25), color="red") + 
    geom_segment(ggplot2::aes(x=1997.75, y=eb_hi_c[5], yend=eb_hi_c[5], xend=1998.25), color="red") +
    geom_segment(ggplot2::aes(x=1998.75, y=eb_hi_c[6], yend=eb_hi_c[6], xend=1999.25), color="red") +
    geom_segment(ggplot2::aes(x=1999.75, y=eb_hi_c[7], yend=eb_hi_c[7], xend=2000.25), color="red") +
    geom_segment(ggplot2::aes(x=2000.75, y=eb_hi_c[8], yend=eb_hi_c[8], xend=2001.25), color="red") +
    geom_segment(ggplot2::aes(x=2001.75, y=eb_hi_c[9], yend=eb_hi_c[9], xend=2002.25), color="red") +
    geom_segment(ggplot2::aes(x=2002.75, y=eb_hi_c[10], yend=eb_hi_c[10], xend=2003.25), color="red") +
    geom_segment(ggplot2::aes(x=2003.75, y=eb_hi_c[11], yend=eb_hi_c[11], xend=2004.25), color="red") + 
    geom_segment(ggplot2::aes(x=2004.75, y=eb_hi_c[12], yend=eb_hi_c[12], xend=2005.25), color="red") +
    geom_segment(ggplot2::aes(x=2005.75, y=eb_hi_c[13], yend=eb_hi_c[13], xend=2006.25), color="red") +
    geom_segment(ggplot2::aes(x=2006.75, y=eb_hi_c[14], yend=eb_hi_c[14], xend=2007.25), color="red") +
    geom_segment(ggplot2::aes(x=2007.75, y=eb_hi_c[15], yend=eb_hi_c[15], xend=2008.25), color="red") +
    geom_segment(ggplot2::aes(x=2008.75, y=eb_hi_c[16], yend=eb_hi_c[16], xend=2009.25), color="red") +
    geom_segment(ggplot2::aes(x=2009.75, y=eb_hi_c[17], yend=eb_hi_c[17], xend=2010.25), color="red") +
    geom_segment(ggplot2::aes(x=2010.75, y=eb_hi_c[18], yend=eb_hi_c[18], xend=2011.25), color="red") +
    geom_segment(ggplot2::aes(x=2011.75, y=eb_hi_c[19], yend=eb_hi_c[19], xend=2012.25), color="red") +
    geom_segment(ggplot2::aes(x=2012.75, y=eb_hi_c[20], yend=eb_hi_c[20], xend=2013.25), color="red") +
    geom_segment(ggplot2::aes(x=2013.75, y=eb_hi_c[21], yend=eb_hi_c[21], xend=2014.25), color="red") +
    geom_segment(ggplot2::aes(x=2014.75, y=eb_hi_c[22], yend=eb_hi_c[22], xend=2015.25), color="red") +
    geom_segment(ggplot2::aes(x=2015.75, y=eb_hi_c[23], yend=eb_hi_c[23], xend=2016.25), color="red") +
    geom_segment(ggplot2::aes(x=2016.75, y=eb_hi_c[24], yend=eb_hi_c[24], xend=2017.25), color="red") +
    geom_segment(ggplot2::aes(x=2017.75, y=eb_hi_c[25], yend=eb_hi_c[25], xend=2018.25), color="red") +
    geom_segment(ggplot2::aes(x=2018.75, y=eb_hi_c[26], yend=eb_hi_c[26], xend=2019.25), color="red") +
    geom_segment(ggplot2::aes(x=2019.75, y=eb_hi_c[27], yend=eb_hi_c[27], xend=2020.25), color="red") +
    geom_segment(ggplot2::aes(x=2020.75, y=eb_hi_c[28], yend=eb_hi_c[28], xend=2021.25), color="red") +
    geom_segment(ggplot2::aes(x=2021.75, y=eb_hi_c[29], yend=eb_hi_c[29], xend=2022.25), color="red") +
    geom_segment(ggplot2::aes(x=2022.75, y=eb_hi_c[30], yend=eb_hi_c[30], xend=2023.25), color="red") +
    geom_segment(ggplot2::aes(x=2023.75, y=eb_hi_c[31], yend=eb_hi_c[31], xend=2024.25), color="red") +
    geom_segment(ggplot2::aes(x=2024.75, y=eb_hi_c[32], yend=eb_hi_c[32], xend=2025.25), color="red")
  p2 <- ggplot2::ggplot(salcha_n, aes(x=year, y=run, fill=Age))+geom_col()+theme_classic()+
    ggtitle("Salcha River -- Total Run by Age") + ylab("Number of Chinook") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks=1994:final_year)+
    geom_segment(aes(x=1994, y=eb_lo_s[1], yend=eb_hi_s[1], xend=1994), color="red") +
    geom_segment(aes(x=1995, y=eb_lo_s[2], yend=eb_hi_s[2], xend=1995), color="red") +
    geom_segment(aes(x=1996, y=eb_lo_s[3], yend=eb_hi_s[3], xend=1996), color="red") +
    geom_segment(aes(x=1997, y=eb_lo_s[4], yend=eb_hi_s[4], xend=1997), color="red") + 
    geom_segment(aes(x=1998, y=eb_lo_s[5], yend=eb_hi_s[5], xend=1998), color="red") +
    geom_segment(aes(x=1999, y=eb_lo_s[6], yend=eb_hi_s[6], xend=1999), color="red") +
    geom_segment(aes(x=2000, y=eb_lo_s[7], yend=eb_hi_s[7], xend=2000), color="red") +
    geom_segment(aes(x=2001, y=eb_lo_s[8], yend=eb_hi_s[8], xend=2001), color="red") +
    geom_segment(aes(x=2002, y=eb_lo_s[9], yend=eb_hi_s[9], xend=2002), color="red") +
    geom_segment(aes(x=2003, y=eb_lo_s[10], yend=eb_hi_s[10], xend=2003), color="red") +
    geom_segment(aes(x=2004, y=eb_lo_s[11], yend=eb_hi_s[11], xend=2004), color="red") + 
    geom_segment(aes(x=2005, y=eb_lo_s[12], yend=eb_hi_s[12], xend=2005), color="red") +
    geom_segment(aes(x=2006, y=eb_lo_s[13], yend=eb_hi_s[13], xend=2006), color="red") +
    geom_segment(aes(x=2007, y=eb_lo_s[14], yend=eb_hi_s[14], xend=2007), color="red") +
    geom_segment(aes(x=2008, y=eb_lo_s[15], yend=eb_hi_s[15], xend=2008), color="red") +
    geom_segment(aes(x=2009, y=eb_lo_s[16], yend=eb_hi_s[16], xend=2009), color="red") +
    geom_segment(aes(x=2010, y=eb_lo_s[17], yend=eb_hi_s[17], xend=2010), color="red") +
    geom_segment(aes(x=2011, y=eb_lo_s[18], yend=eb_hi_s[18], xend=2011), color="red") +
    geom_segment(aes(x=2012, y=eb_lo_s[19], yend=eb_hi_s[19], xend=2012), color="red") +
    geom_segment(aes(x=2013, y=eb_lo_s[20], yend=eb_hi_s[20], xend=2013), color="red") +
    geom_segment(aes(x=2014, y=eb_lo_s[21], yend=eb_hi_s[21], xend=2014), color="red") +
    geom_segment(aes(x=2015, y=eb_lo_s[22], yend=eb_hi_s[22], xend=2015), color="red") +
    geom_segment(aes(x=2016, y=eb_lo_s[23], yend=eb_hi_s[23], xend=2016), color="red") +
    geom_segment(aes(x=2017, y=eb_lo_s[24], yend=eb_hi_s[24], xend=2017), color="red") +
    geom_segment(aes(x=2018, y=eb_lo_s[25], yend=eb_hi_s[25], xend=2018), color="red") +
    geom_segment(aes(x=2019, y=eb_lo_s[26], yend=eb_hi_s[26], xend=2019), color="red") +
    geom_segment(aes(x=2020, y=eb_lo_s[27], yend=eb_hi_s[27], xend=2020), color="red") +
    geom_segment(aes(x=2021, y=eb_lo_s[28], yend=eb_hi_s[28], xend=2021), color="red") +
    geom_segment(aes(x=2022, y=eb_lo_s[29], yend=eb_hi_s[29], xend=2022), color="red") +
    geom_segment(aes(x=2023, y=eb_lo_s[30], yend=eb_hi_s[30], xend=2023), color="red") +
    geom_segment(aes(x=2024, y=eb_lo_s[31], yend=eb_hi_s[31], xend=2024), color="red") +
    geom_segment(aes(x=2025, y=eb_lo_s[32], yend=eb_hi_s[32], xend=2025), color="red") +
    geom_segment(aes(x=1993.75, y=eb_lo_s[1], yend=eb_lo_s[1], xend=1994.25), color="red") +
    geom_segment(aes(x=1994.75, y=eb_lo_s[2], yend=eb_lo_s[2], xend=1995.25), color="red") +
    geom_segment(aes(x=1995.75, y=eb_lo_s[3], yend=eb_lo_s[3], xend=1996.25), color="red") +
    geom_segment(aes(x=1996.75, y=eb_lo_s[4], yend=eb_lo_s[4], xend=1997.25), color="red") + 
    geom_segment(aes(x=1997.75, y=eb_lo_s[5], yend=eb_lo_s[5], xend=1998.25), color="red") +
    geom_segment(aes(x=1998.75, y=eb_lo_s[6], yend=eb_lo_s[6], xend=1999.25), color="red") +
    geom_segment(aes(x=1999.75, y=eb_lo_s[7], yend=eb_lo_s[7], xend=2000.25), color="red") +
    geom_segment(aes(x=2000.75, y=eb_lo_s[8], yend=eb_lo_s[8], xend=2001.25), color="red") +
    geom_segment(aes(x=2001.75, y=eb_lo_s[9], yend=eb_lo_s[9], xend=2002.25), color="red") +
    geom_segment(aes(x=2002.75, y=eb_lo_s[10], yend=eb_lo_s[10], xend=2003.25), color="red") +
    geom_segment(aes(x=2003.75, y=eb_lo_s[11], yend=eb_lo_s[11], xend=2004.25), color="red") + 
    geom_segment(aes(x=2004.75, y=eb_lo_s[12], yend=eb_lo_s[12], xend=2005.25), color="red") +
    geom_segment(aes(x=2005.75, y=eb_lo_s[13], yend=eb_lo_s[13], xend=2006.25), color="red") +
    geom_segment(aes(x=2006.75, y=eb_lo_s[14], yend=eb_lo_s[14], xend=2007.25), color="red") +
    geom_segment(aes(x=2007.75, y=eb_lo_s[15], yend=eb_lo_s[15], xend=2008.25), color="red") +
    geom_segment(aes(x=2008.75, y=eb_lo_s[16], yend=eb_lo_s[16], xend=2009.25), color="red") +
    geom_segment(aes(x=2009.75, y=eb_lo_s[17], yend=eb_lo_s[17], xend=2010.25), color="red") +
    geom_segment(aes(x=2010.75, y=eb_lo_s[18], yend=eb_lo_s[18], xend=2011.25), color="red") +
    geom_segment(aes(x=2011.75, y=eb_lo_s[19], yend=eb_lo_s[19], xend=2012.25), color="red") +
    geom_segment(aes(x=2012.75, y=eb_lo_s[20], yend=eb_lo_s[20], xend=2013.25), color="red") +
    geom_segment(aes(x=2013.75, y=eb_lo_s[21], yend=eb_lo_s[21], xend=2014.25), color="red") +
    geom_segment(aes(x=2014.75, y=eb_lo_s[22], yend=eb_lo_s[22], xend=2015.25), color="red") +
    geom_segment(aes(x=2015.75, y=eb_lo_s[23], yend=eb_lo_s[23], xend=2016.25), color="red") +
    geom_segment(aes(x=2016.75, y=eb_lo_s[24], yend=eb_lo_s[24], xend=2017.25), color="red") +
    geom_segment(aes(x=2017.75, y=eb_lo_s[25], yend=eb_lo_s[25], xend=2018.25), color="red") +
    geom_segment(aes(x=2018.75, y=eb_lo_s[26], yend=eb_lo_s[26], xend=2019.25), color="red") +
    geom_segment(aes(x=2019.75, y=eb_lo_s[27], yend=eb_lo_s[27], xend=2020.25), color="red") +
    geom_segment(aes(x=2020.75, y=eb_lo_s[28], yend=eb_lo_s[28], xend=2021.25), color="red") +
    geom_segment(aes(x=2021.75, y=eb_lo_s[29], yend=eb_lo_s[29], xend=2022.25), color="red") +
    geom_segment(aes(x=2022.75, y=eb_lo_s[30], yend=eb_lo_s[30], xend=2023.25), color="red") +
    geom_segment(aes(x=2023.75, y=eb_lo_s[31], yend=eb_lo_s[31], xend=2024.25), color="red") +
    geom_segment(aes(x=2024.75, y=eb_lo_s[32], yend=eb_lo_s[32], xend=2025.25), color="red") +
    geom_segment(aes(x=1993.75, y=eb_hi_s[1], yend=eb_hi_s[1], xend=1994.25), color="red") +
    geom_segment(aes(x=1994.75, y=eb_hi_s[2], yend=eb_hi_s[2], xend=1995.25), color="red") +
    geom_segment(aes(x=1995.75, y=eb_hi_s[3], yend=eb_hi_s[3], xend=1996.25), color="red") +
    geom_segment(aes(x=1996.75, y=eb_hi_s[4], yend=eb_hi_s[4], xend=1997.25), color="red") + 
    geom_segment(aes(x=1997.75, y=eb_hi_s[5], yend=eb_hi_s[5], xend=1998.25), color="red") +
    geom_segment(aes(x=1998.75, y=eb_hi_s[6], yend=eb_hi_s[6], xend=1999.25), color="red") +
    geom_segment(aes(x=1999.75, y=eb_hi_s[7], yend=eb_hi_s[7], xend=2000.25), color="red") +
    geom_segment(aes(x=2000.75, y=eb_hi_s[8], yend=eb_hi_s[8], xend=2001.25), color="red") +
    geom_segment(aes(x=2001.75, y=eb_hi_s[9], yend=eb_hi_s[9], xend=2002.25), color="red") +
    geom_segment(aes(x=2002.75, y=eb_hi_s[10], yend=eb_hi_s[10], xend=2003.25), color="red") +
    geom_segment(aes(x=2003.75, y=eb_hi_s[11], yend=eb_hi_s[11], xend=2004.25), color="red") + 
    geom_segment(aes(x=2004.75, y=eb_hi_s[12], yend=eb_hi_s[12], xend=2005.25), color="red") +
    geom_segment(aes(x=2005.75, y=eb_hi_s[13], yend=eb_hi_s[13], xend=2006.25), color="red") +
    geom_segment(aes(x=2006.75, y=eb_hi_s[14], yend=eb_hi_s[14], xend=2007.25), color="red") +
    geom_segment(aes(x=2007.75, y=eb_hi_s[15], yend=eb_hi_s[15], xend=2008.25), color="red") +
    geom_segment(aes(x=2008.75, y=eb_hi_s[16], yend=eb_hi_s[16], xend=2009.25), color="red") +
    geom_segment(aes(x=2009.75, y=eb_hi_s[17], yend=eb_hi_s[17], xend=2010.25), color="red") +
    geom_segment(aes(x=2010.75, y=eb_hi_s[18], yend=eb_hi_s[18], xend=2011.25), color="red") +
    geom_segment(aes(x=2011.75, y=eb_hi_s[19], yend=eb_hi_s[19], xend=2012.25), color="red") +
    geom_segment(aes(x=2012.75, y=eb_hi_s[20], yend=eb_hi_s[20], xend=2013.25), color="red") +
    geom_segment(aes(x=2013.75, y=eb_hi_s[21], yend=eb_hi_s[21], xend=2014.25), color="red") +
    geom_segment(aes(x=2014.75, y=eb_hi_s[22], yend=eb_hi_s[22], xend=2015.25), color="red") +
    geom_segment(aes(x=2015.75, y=eb_hi_s[23], yend=eb_hi_s[23], xend=2016.25), color="red") +
    geom_segment(aes(x=2016.75, y=eb_hi_s[24], yend=eb_hi_s[24], xend=2017.25), color="red") +
    geom_segment(aes(x=2017.75, y=eb_hi_s[25], yend=eb_hi_s[25], xend=2018.25), color="red") +
    geom_segment(aes(x=2018.75, y=eb_hi_s[26], yend=eb_hi_s[26], xend=2019.25), color="red") +
    geom_segment(aes(x=2019.75, y=eb_hi_s[27], yend=eb_hi_s[27], xend=2020.25), color="red") +
    geom_segment(aes(x=2020.75, y=eb_hi_s[28], yend=eb_hi_s[28], xend=2021.25), color="red") +
    geom_segment(aes(x=2021.75, y=eb_hi_s[29], yend=eb_hi_s[29], xend=2022.25), color="red") +
    geom_segment(aes(x=2022.75, y=eb_hi_s[30], yend=eb_hi_s[30], xend=2023.25), color="red") +
    geom_segment(aes(x=2023.75, y=eb_hi_s[31], yend=eb_hi_s[31], xend=2024.25), color="red") +
    geom_segment(aes(x=2024.75, y=eb_hi_s[32], yend=eb_hi_s[32], xend=2025.25), color="red")
  p3 <- ggplot2::ggplot(chena_p, aes(x=year, y=run, fill=Age))+geom_col()+theme_classic()+
    ggtitle("Chena River -- Age at Maturity") + ylab("Proportion") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks=1994:final_year)
  p4 <- ggplot2::ggplot(salcha_p, aes(x=year, y=run, fill=Age))+geom_col()+theme_classic()+
    ggtitle("Salcha River -- Age at Maturity") + ylab("Proportion") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
      legend.position = "bottom",
      legend.title = element_text(size=14, face="bold", vjust = 1)
    ) +
    scale_x_continuous(breaks=1994:final_year)
  suppressWarnings(print(ggpubr::ggarrange(p1, p2, p3, p4,
                                           ncol = 1, nrow = 4, legend="right", common.legend = T)))
  dev.off()
}
