#English data, from Wrigley et al. 1997, 290 Tab. 6.19 (Adult mortality, sexes combined (1000qx)), 1640-1809, for age groups 25-84 years
#eng_mort <- read.table(file.choose(), header=TRUE, sep = "\t")
eng_mort <- read.table("/Users/Nils/Documents/Aktuelle_Dokumente/global_history_of_health/global_history/wrigley_et_al_1997_england_1640-1809.txt", header=TRUE, sep = "\t")

eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "Age", value.name = "qx")

years <- unique(eng_mort_melt$variable)

eng_mort_result <- data.frame()
for(i in years) {
  year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
  year_data$age_mod <- year_data$Age - 22.5
  
  # calculation of lx
  lx_c <- NULL
  lx <- 1
  for (k in 2:length(year_data$Age)) {
    lx_1 <- lx * (1 - year_data$qx[k-1]* 0.001)
    lx_c <- c(lx_c, lx_1)
    lx <- lx_1
  }
  year_data$lx <- c(1, lx_c)
  
  # mort_fit_OLS <- lm(log(qx*0.001)  ~ age_mod, data = year_data)
  # OLS_Gompertz_shape <- mort_fit_OLS$coefficients[2]
  # OLS_Gompertz_rate <- exp(mort_fit_OLS$coefficients[1])
  
  # fit survival data with nls
  nls_estim_fit <- nls(lx ~ exp(a/b - a/b * exp(b * (Age - 25) ) ) , 
                       data = year_data, start=list(a = 0.001, b = 0.075) )
  NLS_estim_Gompertz_shape <- summary(nls_estim_fit)$coefficients[2]
  NLS_estim_Gompertz_rate <- summary(nls_estim_fit)$coefficients[1]
  
  ind_result <- cbind(year = substring(i, 2), beta = NLS_estim_Gompertz_shape, alpha = NLS_estim_Gompertz_rate)
  eng_mort_result <- rbind(eng_mort_result, ind_result )
}

rownames(eng_mort_result) <- NULL
cols.num <- c("year", "beta", "alpha")
eng_mort_result[cols.num] <- sapply(eng_mort_result[cols.num],as.numeric)