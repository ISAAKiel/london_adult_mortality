# London data, from Landers 1997, 172 Tab. 5.5
eng_mort <- read.table("./data/Landers_1997_London.txt", header=TRUE, sep = "\t")
eng_mort <- eng_mort[-1,]
eng_mort_melt <- reshape2::melt(eng_mort, id.vars = "Age", value.name = "qx")

eng_mort_melt$mx <- ifelse(eng_mort_melt$Age < 60, 1 / (15 / eng_mort_melt$qx - 7.5), 1 / (20 / eng_mort_melt$qx - 10) )
years <- unique(eng_mort_melt$variable)

eng_mort_result <- data.frame()
for(i in years) {
  year_data <- eng_mort_melt[ which(eng_mort_melt$variable == i), ]
  year_data$age_mod <- year_data$Age - 15
  
  df_length <- length(year_data$age_mod)
  
  # calculation of dx
  dx <- NULL
  lx_ <- NULL
  lx <- 1
  for (k in 1: df_length ) {
    dx_1 <- year_data$qx[k] * lx
    lx <- lx - dx_1
    dx <- c(dx, dx_1)
    lx_ <- c(lx_, lx)
  }
  year_data$dx <- dx
  

  
  # Bayes
  year_data_uncount <- year_data %>% uncount(round(dx * 100))
  year_data_uncount$age_end <- ifelse(year_data_uncount$Age < 60, year_data_uncount$Age + 14, year_data_uncount$Age + 39)


  gomp.anthr_age(year_data_uncount, age_beg = "Age", age_end = "age_end",
                 silent.jags = FALSE,
                 silent.runjags = FALSE,
                 thinSteps = 10,
                 numSavedSteps = 100000,
                 minimum_age = 15) %>%
    diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
  bayes_anthr_gomp_b <- gomp_anthr_MCMC_diag[2,5]
  bayes_anthr_gomp_a <- gomp_anthr_MCMC_diag[1,5]
  
  # ind_result <- cbind(year = substring(i, 2), beta = sample_data_lt_Gompertz_shape, alpha = sample_data_lt_Gompertz_rate)
  ind_result <- cbind(year = substring(i, 2), beta = OLS_Gompertz_shape, alpha = OLS_Gompertz_rate)
  # ind_result <- cbind(year = substring(i, 2), beta = MLE_estim_lt_Gompertz_shape, alpha = MLE_estim_lt_Gompertz_rate)
  # ind_result <- cbind(year = substring(i, 2), beta = bayes_anthr_gomp_b, alpha = bayes_anthr_gomp_a)
  
  eng_mort_result <- rbind(eng_mort_result, ind_result )
}

rownames(eng_mort_result) <- NULL
cols.num <- c("year", "beta", "alpha")
eng_mort_result[cols.num] <- sapply(eng_mort_result[cols.num],as.numeric)

ggplot(eng_mort_melt[-c(4, 8),],aes(x = Age, y = log(mx), group = variable, colour = variable)) + geom_point() +
  geom_smooth(method='lm', formula= y~x)
ggplot(year_data,aes(x = Age, y = dx, group = variable, colour = variable)) + geom_point()
