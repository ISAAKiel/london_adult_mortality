# data from Perrenoud 1978
geneva <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/data/perrenoud_geneva.txt", header=TRUE, sep = "\t")

geneva_melt <- reshape2::melt(geneva, id.vars = "age", value.name = "lx")

years <- unique(geneva_melt$variable)

geneva_result <- data.frame()
for(i in years) {
  year_data <- geneva_melt[ which(geneva_melt$variable == i & geneva_melt$age >= 15), ]
  year_data$age_mod <- year_data$age - 15
  year_data$lx_mod <- year_data$lx / year_data$lx[1]
 
  df_length <- length(year_data$age)
  
  # calculation of dx
  dx <- NULL
  for (k in 1:(df_length - 1) ) {
    dx_1 <- year_data$lx_mod[k] - year_data$lx_mod[k+1]
    dx <- c(dx, dx_1)
  }
  year_data$dx <- c(dx, year_data$lx_mod[df_length])
  
  year_data$death <- 1
  year_data_lt <- flexsurv::flexsurvreg(formula = survival::Surv(age_mod, death) ~ 1, 
                                        data = year_data, dist="gompertz", weights = dx)
  year_data_lt_Gompertz_shape <- year_data_lt$coefficients[1]
  year_data_lt_Gompertz_rate <- exp(year_data_lt$coefficients[2])
  
  year_begin <- as.numeric(substring(i, 2, 5))
  year_end <- as.numeric(substring(i, 7, 10))
  year_mid <- (year_begin + year_end) / 2
  
  ind_result <- cbind(year = year_mid, beta = year_data_lt_Gompertz_shape, alpha = year_data_lt_Gompertz_rate)
  geneva_result <- rbind(geneva_result, ind_result )
}

rownames(geneva_result) <- NULL
cols.num <- c("year", "beta", "alpha")
geneva_result[cols.num] <- sapply(geneva_result[cols.num],as.numeric)