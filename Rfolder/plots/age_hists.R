#' Escapement by Age Histogram
#' @param samples (data.frame) output of clean_chains
#' @param file_path  (character) file path to store output jpeg file.
#' @param width (numeric) width of the output plot (in px).
#' @param height (numeric) height of the output plot (in px).
#' @export
age_hist <- function(samples, file_path, width=500, height=1000, final_year=2020){
  
  aba_samples <- samples[,substr(names(samples), 1, 3)=="N_1" &
                           substr(names(samples), 4, 4)!="_"]
  quants <- apply(aba_samples, MARGIN=2, quantile, probs = c(0.5))
  med_array <- array(NA, dim=c(length(1986:final_year), 2, 6))
  l <- 1
  for(k in 1:6){
    for(j in 1:2){
      for(i in 1:length(1986:final_year)){
        med_array[i, j, k] <- quants[l] 
        l <- l + 1
      }
    }
  }
  chena_tab <- salcha_tab <- data.frame(
    year=1994:final_year,
    Age_3=rep(NA, length(1994:final_year)),
    Age_4=rep(NA, length(1994:final_year)),
    Age_5=rep(NA, length(1994:final_year)),
    Age_6=rep(NA, length(1994:final_year)),
    Age_7=rep(NA, length(1994:final_year)),
    Age_8=rep(NA, length(1994:final_year))
  )
  for (i in 1:length(1994:final_year)){
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
  med_array <-  array(NA, dim=c(length(1986:final_year), 2, 6))
  l <- 1
  for(k in 1:6){
    for(j in 1:2){
      for(i in 1:length(1986:final_year)){
        med_array[i, j, k] <- quants[l] 
        l <- l + 1
      }
    }
  }
  chena_tab <- salcha_tab <- data.frame(
    year=1986:final_year,
    Age_3=rep(NA, length(1986:final_year)),
    Age_4=rep(NA, length(1986:final_year)),
    Age_5=rep(NA, length(1986:final_year)),
    Age_6=rep(NA, length(1986:final_year)),
    Age_7=rep(NA, length(1986:final_year)),
    Age_8=rep(NA, length(1986:final_year))
  )
  for (i in 1:length(1986:final_year)){
    for (k in 1:6){
      chena_tab[i, k+1] <- med_array[i, 1, k]
      salcha_tab[i, k+1] <- med_array[i, 2, k]
    }
  }
  chena_tab <- chena_tab[9:nrow(chena_tab),]
  salcha_tab <- salcha_tab[9:nrow(salcha_tab),]
  chena_norm <- apply(chena_tab[,2:ncol(chena_tab)], MARGIN=1, sum)
  salcha_norm <- apply(salcha_tab[,2:ncol(salcha_tab)], MARGIN=1, sum)
  for(i in 1:nrow(chena_tab)){
    for(j in 2:ncol(chena_tab)){
      chena_tab[i,j] <- chena_tab[i,j]/chena_norm[i]
      salcha_tab[i,j] <- salcha_tab[i,j]/salcha_norm[i]
    }
  }
  chena_p <- data.frame(year=rep(1994:final_year, rep(6, length(1994:final_year))),
                        Age=rep(1:6, length(1994:final_year)),
                        run=rep(NA, 6*length(1994:final_year)))
  salcha_p <- data.frame(year=rep(1994:final_year, rep(6, length(1994:final_year))),
                         Age=rep(1:6, length(1994:final_year)),
                         run=rep(NA, 6*length(1994:final_year)))
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
  p1 <- ggplot2::ggplot(chena_n, aes(x=year, y=run, fill=Age))+geom_col()+theme_classic()+
      ggtitle("Chena River -- Total Run by Age") + ylab("Number of Chinook") +
      theme(
        plot.title = element_text(size=16, face="bold", hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
        legend.position = "none"
      ) +
      scale_x_continuous(breaks=1994:final_year)
  p2 <- ggplot2::ggplot(salcha_n, aes(x=year, y=run, fill=Age))+geom_col()+theme_classic()+
    ggtitle("Salcha River -- Total Run by Age") + ylab("Number of Chinook") +
    theme(
      plot.title = element_text(size=16, face="bold", hjust=0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=14),
      axis.text.x = element_text(angle=90, vjust=0.5,hjust=1),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks=1994:final_year)
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
