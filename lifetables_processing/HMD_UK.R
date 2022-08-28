if (runCodeNew){
  set.seed(689)
  HMD_username <- readline(prompt = "Enter username: ")
  HMD_password <- readline(prompt="Enter password: ")
  credentials <- c(HMD_username, HMD_password)
  
  # HMDHFDplus::getHMDitemavail("GBRTENW", credentials[1], credentials[2])
  
  # get dx
  uk_dx <- HMDHFDplus::readHMDweb("GBRTENW", "bltper_5x5", credentials[1], credentials[2])
  
  HMD_UK_result <- data.frame()
  years <- unique(uk_dx$Year)
  years <- years[years <= 1900]
  for(i in years) {
    year_data <- uk_dx[ which(uk_dx$Year == i), ]
    year_data <- subset(year_data, Age >= 15)
    # year_data$death <- 1 
    # surv_lt <- flexsurv::flexsurvreg(formula = survival::Surv(Age - 15, death) ~ 1, data = year_data, dist="gompertz", weights = dx)
    # surv_lt_Gompertz_shape <- surv_lt$coefficients[1]
    # surv_lt_Gompertz_rate <- exp(surv_lt$coefficients[2])
    # ind_result <- cbind(year = i, beta = surv_lt_Gompertz_shape, alpha = surv_lt_Gompertz_rate)
    
    # year_data$Age_mod <- year_data$Age - 15
    # year_data$Age_end <- year_data$Age_mod + 5
    # MLE_lt <- Gomp.MLE.interval(year_data, agebegin = "Age_mod", ageend = "Age_end", Dx = "dx")
    # MLE_lt_Gompertz_shape <- MLE_lt[2]
    # MLE_lt_Gompertz_rate <- MLE_lt[1]
    # ind_result <- cbind(year = i, beta = MLE_lt_Gompertz_shape, alpha = MLE_lt_Gompertz_rate)
    
    mort_df_x_uncount <- year_data %>% uncount(round(dx / 100))
    mort_df_x_uncount$Age_end <-  mort_df_x_uncount$Age + 5
    gomp.anthr_age(mort_df_x_uncount, age_beg = "Age", age_end = "Age_end",
                   silent.jags = FALSE,
                   silent.runjags = FALSE,
                   thinSteps = 1,
                   numSavedSteps = 300000,
                   minimum_age = 15) %>%
      diagnostic.summary(., HDImass = 0.95) -> gomp_anthr_MCMC_diag
    
    ind_result <- cbind(year =  paste0("X", i), parameter = c("alpha", "beta", "M"), gomp_anthr_MCMC_diag[1:3,])
    rownames(ind_result) <- NULL
    
    HMD_UK_result <- rbind(HMD_UK_result, ind_result )
  }
  # saves results in Rda-object
  save(HMD_UK_result, file = file.path(".", saveFileDir, "HMD_UK_result.Rda") )
}
load(file.path(".", saveFileDir, "HMD_UK_result.Rda") )

# range of Gompertz beta values
beta_range <- paste0(round(min(HMD_UK_result[which(HMD_UK_result$parameter == "beta"),]$Mode), 4), "-",
                     round(max(HMD_UK_result[which(HMD_UK_result$parameter == "beta"),]$Mode) , 4) )
# range of age modes M
M_range <- paste0(round(min(HMD_UK_result[which(HMD_UK_result$parameter == "M"),]$Mode), 1), "-",
                  round(max(HMD_UK_result[which(HMD_UK_result$parameter == "M"),]$Mode), 1) )

HMD_UK_ranges <- data.frame(parameter = c("beta", "M"), ranges = c(beta_range, M_range))
