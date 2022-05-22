# data from Perrenoud 1978
geneva <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/perrenoud_geneva.txt", header=TRUE, sep = "\t")

geneva_melt <- reshape2::melt(geneva, id.vars = "age", value.name = "lx")

years <- unique(geneva_melt$variable)

geneva_result <- data.frame()
for(i in years) {
  year_data <- geneva_melt[ which(geneva_melt$variable == i & geneva_melt$age >= 15), ]
  year_data$age_mod <- year_data$age - 15
  year_data$lx_mod <- year_data$lx / year_data$lx[1]
  
  # fit survival data with nls
  nls_estim_fit <- nls(lx_mod ~ exp(a/b - a/b * exp(b * age_mod ) ) , 
                       data = year_data, start=list(a = 0.001, b = 0.075) )
  NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
  NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
  
  year_begin <- as.numeric(substring(i, 2, 5))
  year_end <- as.numeric(substring(i, 7, 10))
  year_mid <- (year_begin + year_end) / 2
  
  ind_result <- cbind(year = year_mid, beta = NLS_estim_Gompertz_shape, alpha = NLS_estim_Gompertz_rate)
  geneva_result <- rbind(geneva_result, ind_result )
}

rownames(geneva_result) <- NULL
cols.num <- c("year", "beta", "alpha")
geneva_result[cols.num] <- sapply(geneva_result[cols.num],as.numeric)